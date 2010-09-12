{-# OPTIONS -XMultiParamTypeClasses #-} 
{-# OPTIONS -XTypeSynonymInstances #-} 

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
rtest = Rectangle 0 0 0 0

data HMPCState = HMPCState 
    { current_screen    :: Screen
    , screens           :: [Screen]
    , global_keys       :: M.Map C.Key (IO ())
    }
hmpctest = HMPCState "a" ["a","b","c","d"] (M.fromList keyList)

keyList = 
    [(C.KeyChar 'q',    exitHmpc)
    ,(C.KeyChar 'd',    drawTestWidget testWidget)
    ,(C.KeyChar 'h',    drawTestWidget testWidget2)
    ] 

data SimpleLayout = SimpleLayout

class Layout l a where
    runLayout :: l -> [a] -> Rectangle -> StateT HMPCState IO ()

instance Layout SimpleLayout Widget where
    runLayout _ xs r = mapM_ (liftIO . putStrLn) xs

type Screen = String 
type Widget = String
type HMPC = StateT HMPCState IO

wtest :: [Widget]
wtest = ["a","b","c","d"]

exitHmpc = CH.end >> exitSuccess

drawTestWidget = W.drawTextWidget (0,0) (10,10) W.DHNormal 
testWidget = W.TextWidget "test" 0 0 W.defaultTWOptions
testWidget2 = W.TextWidget "test2" 0 0 W.defaultTWOptions

processKey :: C.Key -> HMPC ()
processKey k = do si <- get
                  let a = M.lookup k (global_keys si)
                  let sk = CH.displayKey k
                  liftIO $ maybe (return ()) (liftM id) a

switchScreen :: Int -> HMPC Screen
switchScreen n = do si <- get
                    let ss = screens si
                    let s = ss !! n
                    put (si { screens = ss, current_screen = s})
                    return s

screenAction k = processKey k >> switchScreen 2 >> displayLayout
displayLayout = (runLayout SimpleLayout wtest ) rtest

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
