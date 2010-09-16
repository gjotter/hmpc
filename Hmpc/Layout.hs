{-# OPTIONS -XExistentialQuantification #-}
{-# OPTIONS -XMultiParamTypeClasses #-} 
{-# OPTIONS -XFlexibleInstances #-}

module Hmpc.Layout
    ( StackLayout (..)
    , SimpleScreen (..)
    ) where

import Hmpc.Core
import Hmpc.Widgets()

data StackLayout a = StackLayout

data SimpleScreen = SimpleScreen
    { smpl_widgets   :: ![Drawable]
    , smpl_layout    :: !(Layout Drawable)
    }

instance LayoutClass StackLayout Drawable where
    runLayout StackLayout xs r = sequence actions 
                               where actions = zipWith ($) (map draw rects) xs
                                     rects   = stackHelper (length xs) r

stackHelper :: Int -> Rectangle -> [Rectangle]
stackHelper n r = snd . unzip $ iterate (generateStackRectangles step) (remain,r')
                where step    = floor $ (fromIntegral $ rect_height r) / (fromIntegral n)
                      remain  = (rect_height r) `mod` n
                      r'      = r { rect_height = step + y_shift }
                      y_shift | remain > 0 = 1
                              | otherwise  = 0

generateStackRectangles :: Int -> (Int,Rectangle) -> (Int,Rectangle)
generateStackRectangles s (m,r) = (m - 1,r')
                                where r' = r { rect_y      = s + (rect_y r) + m1
                                             , rect_height = s + m1 }
                                      m1 | m > 0 = 1
                                         | otherwise    = 0

instance ScreenClass SimpleScreen where
    displayScreen s r = do w' <- runLayout (smpl_layout s) (smpl_widgets s) r
                           return s { smpl_widgets = w' }




