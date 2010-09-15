module Hmpc.Widgets
    ( drawTextWidget
    , getArtists
    ) where

import Hmpc.Core
import Control.Monad.Trans 
import qualified Network.MPD as MPD

import qualified UI.HSCurses.Widgets as W
import qualified Data.Map as M
import Data.List (intercalate)

instance DrawableClass W.TextWidget where
    draw r w = drawTextWidget r w >>= return

data PlaylistWidget = PlaylistWidget 
    { pl_textwidget     :: W.TextWidget
    , pl_index          :: Integer
    , pl_pos            :: Integer
    , pl_songs          :: [MPD.Song]
    , pl_drawhint       :: W.DrawingHint
    }

drawPlaylistWidget :: Rectangle -> PlaylistWidget -> HMPC PlaylistWidget
drawPlaylistWidget r pl = do s <- MPD.status
                             let pl_id = MPD.stPlaylistID s
                             let pl_id' = pl_index pl
                             if (pl_songs pl) == [] || pl_id > pl_id'
                                then updatePlaylistWidget r pl
                                else drawPlaylistWidget' r pl

drawPlaylistWidget' :: Rectangle -> PlaylistWidget -> HMPC PlaylistWidget
drawPlaylistWidget' r pl = do let n = rect_height r 
                              let songs = take n $ pl_songs pl
                              let as = intercalate "\n" $ getArtists songs
                              let w' = W.textWidgetSetText (pl_textwidget pl) as
                              return pl { pl_textwidget = w' }

updatePlaylistWidget :: Rectangle -> PlaylistWidget -> HMPC PlaylistWidget
updatePlaylistWidget r pl = do songs <- MPD.playlistInfo Nothing
                               status <- MPD.status
                               let pl_id = MPD.stPlaylistID status
                               let pl' = pl { pl_songs = songs, pl_index = pl_id }
                               drawPlaylistWidget' r pl'

drawTextWidget :: Rectangle -> W.TextWidget -> HMPC W.TextWidget
drawTextWidget r w = do pl <- MPD.playlistInfo (Just (0,rect_height r))
                        let w' = W.textWidgetSetText w $ intercalate "\n" $ getArtists pl
                        liftIO $ W.drawTextWidget pos size W.DHNormal w'
                        return w'
                        where pos = (rect_y r, rect_x r)
                              size = (rect_height r, rect_width r)

getArtists :: [MPD.Song] -> [String]
getArtists ss = concat $ map (maybe [] id) $ map (M.lookup MPD.Artist) $ map MPD.sgTags ss
