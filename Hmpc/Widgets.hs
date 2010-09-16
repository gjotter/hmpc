module Hmpc.Widgets
    ( drawTextWidget
    , getArtists
    , PlaylistWidget (..)
    ) where

import Hmpc.Core
import Control.Monad.Trans 
import qualified Network.MPD as MPD

import qualified UI.HSCurses.Widgets as W
import qualified UI.HSCurses.Curses as C
import qualified Data.Map as M
import Data.List (intercalate)

instance DrawableClass W.TextWidget where
    draw r w = drawTextWidget r w >>= return

instance DrawableClass PlaylistWidget where
    draw r w = drawPlaylistWidget r w >>= return

data PlaylistWidget = PlaylistWidget 
    { pl_textwidget     :: !W.TextWidget
    , pl_id             :: !Integer
    , pl_pos            :: !Int
    , pl_bufferpos      :: !Int
    , pl_buffer         :: !Int
    , pl_songs          :: ![MPD.Song]
    , pl_drawhint       :: !W.DrawingHint
    }

drawPlaylistWidget :: Rectangle -> PlaylistWidget -> HMPC PlaylistWidget
drawPlaylistWidget r pl = do s <- MPD.status
                             let pid  = MPD.stPlaylistID s
                                 pid' = pl_id pl
                             if (((pl_songs pl) == []) || (pid > pid'))
                                then updatePlaylistWidget r pid pl
                                else drawPlaylistWidget' r pl

drawPlaylistWidget' :: Rectangle -> PlaylistWidget -> HMPC PlaylistWidget
drawPlaylistWidget' r pl = do liftIO $ W.drawTextWidget pos size (pl_drawhint pl) w'
                              return (pl { pl_textwidget = w' })
                              where w'    = W.newTextWidget wo as
                                    wo    = W.TWOptions (W.TWSizeFixed size) W.defaultDrawingStyle W.AlignLeft
                                    as    = (intercalate "\n" $ getArtists songs)
                                    songs = pl_songs pl
                                    pos   = (rect_y r, rect_x r)
                                    size  = (rect_height r, rect_width r)
                                    

updatePlaylistWidget :: Rectangle -> Integer -> PlaylistWidget -> HMPC PlaylistWidget
updatePlaylistWidget r pid pl = do songs <- MPD.playlistInfo $ Just (pl_pos pl, rect_height r)
                                   let pl' = pl { pl_songs = songs, pl_id = pid }
                                   drawPlaylistWidget' r pl'

drawTextWidget :: Rectangle -> W.TextWidget -> HMPC W.TextWidget
drawTextWidget r w = do pl <- MPD.playlistInfo (Just (0,rect_height r))
                        let as = intercalate "\n" $ getArtists pl
                            w' = W.textWidgetSetText w as
                        liftIO $ W.drawTextWidget pos size W.DHNormal w'
                        return w'
                        where pos  = (rect_y r, rect_x r)
                              size = (rect_height r, rect_width r)

getArtists :: [MPD.Song] -> [String]
getArtists ss = concat $ map (maybe [] id) maybea
              where maybea = map (M.lookup MPD.Artist) tags
                    tags   = map MPD.sgTags ss
