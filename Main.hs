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
rtest = Rectangle 0 0 20 20

data HMPCState = HMPCState 
    { current_screen    :: Screen
    , screens           :: [Screen]
    , global_keys       :: M.Map C.Key (HMPC ())
    }
hmpctest = HMPCState testScreena testScreens (M.fromList keyList)

keyList = 
    [(C.KeyChar 'q',    exitHmpc)
    ,(C.KeyChar 'j',    screenAction 0)
    ,(C.KeyChar 'k',    screenAction 1)
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
    runLayout SimpleLayout xs r = draw r (head xs)

instance LayoutClass Layout a where
    runLayout (Layout l) = runLayout l


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
    draw r w = drawTestWidget r w


type HMPC = StateT HMPCState IO

exitHmpc = liftIO $ CH.end >> exitSuccess

drawTestWidget r = W.drawTextWidget pos size W.DHNormal 
                    where pos = (rect_x r, rect_y r)
                          size = (rect_width r, rect_height r)

testWidget = W.TextWidget "test" 0 0 W.defaultTWOptions
testWidget2 = W.TextWidget "test2" 0 0 W.defaultTWOptions
testWidgets = map Drawable (testWidget : testWidget2 : [])

processKey :: C.Key -> HMPC ()
processKey k = do si <- get
                  let a = M.lookup k (global_keys si)
                  let sk = CH.displayKey k
                  maybe (return ()) (id) a

switchScreen :: Int -> HMPC Screen
switchScreen n = do si <- get
                    let ss = screens si
                    let s = ss !! n
                    put (si { screens = ss, current_screen = s})
                    return s

screenAction n = do s <- switchScreen n
                    liftIO $ displayScreen s rtest
                    

{-
screenAction k = processKey k >> switchScreen 2 >> displayLayout
displayLayout = (runLayout SimpleLayout wtest ) rtest
-}

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
