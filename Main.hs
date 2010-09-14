{-# OPTIONS -XMultiParamTypeClasses #-} 
{-# OPTIONS -XTypeSynonymInstances #-} 
{-# OPTIONS -XExistentialQuantification #-}
{-# OPTIONS -XFlexibleInstances #-}

import System.Exit

import Control.Monad.State
import qualified Data.Map as M

import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Widgets as W

data Rectangle = Rectangle
    { rect_x        :: Int
    , rect_y        :: Int
    , rect_width    :: Int
    , rect_height   :: Int 
    } deriving (Show, Eq)
rtest = Rectangle 10 0 20 10

data HMPCState = HMPCState 
    { current_screen    :: Screen
    , screens           :: [Screen]
    , global_keys       :: M.Map C.Key (HMPC ())
    }
hmpctest = HMPCState testScreena testScreens (M.fromList keyList)

keyList = 
    [(C.KeyChar 'q',    exitHmpc)
    ,(C.KeyChar 'j',    switchScreen 0)
    ,(C.KeyChar 'k',    switchScreen 1)
    ] 

data SimpleLayout a = SimpleLayout
data SimpleScreen = SimpleScreen
    { widgets   :: [Drawable]
    , layout    :: Layout Drawable
    }

testScreens = testScreena : testScreenb : []
testScreena = Screen $ SimpleScreen testWidgets (Layout SimpleLayout)
testScreenb = Screen $ SimpleScreen (reverse testWidgets) (Layout SimpleLayout)

data Layout a = forall l. (LayoutClass l a) => Layout (l a)

class LayoutClass l a where
    runLayout :: l a -> [a] -> Rectangle -> IO ()

instance LayoutClass SimpleLayout Drawable where
    runLayout SimpleLayout xs r = sequence_ actions 
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


type HMPC = StateT HMPCState IO

exitHmpc = liftIO $ CH.end >> exitSuccess

drawTextWidget r = W.drawTextWidget pos size W.DHNormal 
                   where pos = (rect_y r, rect_x r)
                         size = (rect_height r, rect_width r)

testWidget = W.TextWidget "test" 0 0 W.defaultTWOptions
testWidget2 = W.TextWidget "test2" 0 0 W.defaultTWOptions
testWidgets = map Drawable (testWidget : testWidget2 : [])

processKey :: C.Key -> HMPC ()
processKey k = do si <- get
                  let a = M.lookup k (global_keys si)
                  let sk = CH.displayKey k
                  maybe (return ()) (id) a

switchScreen :: Int -> HMPC ()
switchScreen n = do si <- get
                    let ss = screens si
                    let s = ss !! n
                    put (si { screens = ss, current_screen = s})
                    (h,w) <- liftIO $ C.scrSize
                    let rect = Rectangle 0 0 (w-1) (h)
                    liftIO $ displayScreen s rect

eventloop = do k <- liftIO $ CH.getKey C.refresh
               processKey k
               eventloop
                            
main = do CH.start
          runStateT eventloop hmpctest
          CH.end

instance Ord C.Key where
    x < y = show x < show y
    x <= y = show x <= show y
    x >= y = show x >= show y
    x > y = show x > show y
