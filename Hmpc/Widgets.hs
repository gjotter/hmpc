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
    draw r w = drawTextWidget r w

drawTextWidget :: Rectangle -> W.TextWidget -> HMPC ()
drawTextWidget r w = do pl <- MPD.playlistInfo (Just (0,rect_height r))
                        let w' = W.textWidgetSetText w $ intercalate "\n" $ getArtists pl
                        liftIO $ W.drawTextWidget pos size W.DHNormal w'
                        where pos = (rect_y r, rect_x r)
                              size = (rect_height r, rect_width r)

getArtists :: [MPD.Song] -> [String]
getArtists ss = concat $ map (maybe [] id) $ map (M.lookup MPD.Artist) $ map MPD.sgTags ss