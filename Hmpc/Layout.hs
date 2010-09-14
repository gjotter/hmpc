{-# OPTIONS -XExistentialQuantification #-}
{-# OPTIONS -XMultiParamTypeClasses #-} 
{-# OPTIONS -XFlexibleInstances #-}

module Hmpc.Layout
    ( Rectangle (..)
    , StackLayout (..)
    , SimpleScreen (..)
    , Layout (..)
    , LayoutClass (..)
    , Screen (..)
    , ScreenClass (..)
    , Drawable (..)
    , DrawableClass (..)
    ) where

import qualified UI.HSCurses.Widgets as W

data Rectangle = Rectangle
    { rect_x        :: Int
    , rect_y        :: Int
    , rect_width    :: Int
    , rect_height   :: Int 
    } deriving (Show, Eq)
rtest = Rectangle 10 0 20 10

data StackLayout a = StackLayout
data SimpleScreen = SimpleScreen
    { widgets   :: [Drawable]
    , layout    :: Layout Drawable
    }

data Layout a = forall l. (LayoutClass l a) => Layout (l a)

class LayoutClass l a where
    runLayout :: l a -> [a] -> Rectangle -> IO ()

instance LayoutClass StackLayout Drawable where
    runLayout StackLayout xs r = sequence_ actions 
                                   where actions = zipWith ($) (map draw rects) xs
                                         rects = simpleHelper (length xs) r

instance LayoutClass Layout a where
    runLayout (Layout l) = runLayout l

simpleHelper :: Int -> Rectangle -> [Rectangle]
simpleHelper n r =  snd . unzip $ iterate (generateRectangle step) (remain,r')
                    where step = floor $ (/fromIntegral n) $ fromIntegral $ rect_height r
                          remain = (rect_height r) `mod` n
                          r' = r { rect_height = step + y_shift }
                          y_shift | remain > 0 = 1
                                  | otherwise  = 0

generateRectangle :: Int -> (Int,Rectangle) -> (Int,Rectangle)
generateRectangle y_step (y_remain,r) = (y_remain - 1,r')
                                        where r' = r { rect_y      = y_step + (rect_y r) + y_shift
                                                     , rect_height = y_step 
                                                     }
                                              y_shift | y_remain > 0 = 1
                                                      | otherwise    = 0

data Screen = forall a. (ScreenClass a) => Screen a

class ScreenClass a where
    displayScreen :: a -> Rectangle -> IO ()

instance ScreenClass Screen where
    displayScreen (Screen a) r = displayScreen a r

instance ScreenClass SimpleScreen where
    displayScreen s r = runLayout (layout s) (widgets s) r


data Drawable = forall a. (DrawableClass a) => Drawable a

class DrawableClass a where
    draw :: Rectangle -> a -> IO ()

instance DrawableClass Drawable where
    draw r (Drawable a) = draw r a

instance DrawableClass W.TextWidget where
    draw r w = drawTextWidget r w

drawTextWidget r = W.drawTextWidget pos size W.DHNormal 
                   where pos = (rect_y r, rect_x r)
                         size = (rect_height r, rect_width r)

