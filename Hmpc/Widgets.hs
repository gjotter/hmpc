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
    { pl_tw             :: !W.TextWidget
    , pl_twstyle        :: !W.DrawingStyle
    , pl_twalign        :: !W.HAlignment
    , pl_twdrawhint     :: !W.DrawingHint
    , pl_size           :: !Size
    , pl_id             :: !Integer
    , pl_pos            :: !Int
    , pl_bufferpos      :: !Int
    , pl_buffer         :: !(Maybe Int)
    , pl_songs          :: ![MPD.Song]
    , pl_display        :: ![MPD.Metadata]
    }

type Size = (Int,Int)

drawPlaylistWidget :: Rectangle -> PlaylistWidget -> HMPC PlaylistWidget
drawPlaylistWidget r pl = do s <- MPD.status
                             let pid  = MPD.stPlaylistID s
                                 pid' = pl_id pl
                             if (((pl_songs pl) == []) || (pid > pid'))
                                then updatePlaylistWidget r pid pl
                                else drawPlaylistWidget' r pl

drawPlaylistWidget' :: Rectangle -> PlaylistWidget -> HMPC PlaylistWidget
drawPlaylistWidget' r pl = do liftIO $ W.drawTextWidget pos size (pl_twdrawhint pl) (pl_tw pl')
                              return pl'
                              where pl'  = updatePlaylistTextWidget pl size
                                    pos  = (rect_y r, rect_x r)
                                    size = (rect_height r, rect_width r)
                             
updatePlaylistWidget :: Rectangle -> Integer -> PlaylistWidget -> HMPC PlaylistWidget
updatePlaylistWidget r pid pl = do songs <- MPD.playlistInfo $ playlistBufferSize pl r
                                   let pl' = pl { pl_songs = songs, pl_id = pid }
                                   drawPlaylistWidget' r pl'

updatePlaylistTextWidget :: PlaylistWidget -> Size -> PlaylistWidget 
updatePlaylistTextWidget pl s | s /= s' = pl { pl_tw = W.newTextWidget wo str, pl_size = s' }
                              | otherwise    = pl
                              where wo  = W.TWOptions (W.TWSizeFixed s) (pl_twstyle pl) (pl_twalign pl)
                                    str = playlistSongs pl
                                    s'  = pl_size pl

playlistBufferSize :: PlaylistWidget -> Rectangle -> Maybe Size
playlistBufferSize pl r = maybe (Nothing) (Just . size) $ pl_buffer pl
                        where size bs = (toZero $ (pl_pos pl) - bs, (rect_height r) + bs)

playlistSongs :: PlaylistWidget -> String
playlistSongs pl = intercalate "\n" $ map (playlistSong pl) (pl_songs pl) 

playlistSong :: PlaylistWidget -> MPD.Song -> String
playlistSong pl s = intercalate " - " $ concat $ map (maybe [] id) dt
                  where dt = map (\x -> x t) $ map M.lookup (pl_display pl) 
                        t  = MPD.sgTags s

drawTextWidget :: Rectangle -> W.TextWidget -> HMPC W.TextWidget
drawTextWidget r w = do pl <- MPD.playlistInfo (Just (0,rect_height r))
                        let as = intercalate "\n" $ getArtists pl
                            w' = W.textWidgetSetText w as
                        liftIO $ W.drawTextWidget pos size W.DHNormal w'
                        return w'
                        where pos  = (rect_y r, rect_x r)
                              size = (rect_height r, rect_width r)

toZero :: (Ord a, Num a) => a -> a
toZero n | n < 0 = 0
         | otherwise = n

getArtists :: [MPD.Song] -> [String]
getArtists ss = concat $ map (maybe [] id) maybea
              where maybea = map (M.lookup MPD.Artist) tags
                    tags   = map MPD.sgTags ss
