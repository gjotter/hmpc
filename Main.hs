{-# OPTIONS -XMultiParamTypeClasses #-} 
{-# OPTIONS -XTypeSynonymInstances #-} 

import Control.Monad.State
import qualified Data.Map as M

data Rectangle = Rectangle
    { rect_x        :: Int
    , rect_y        :: Int
    , rect_width    :: Int
    , rect_height   :: Int 
    } deriving (Show, Eq)
rtest = Rectangle 0 0 0 0

data Key = Keyx | Keyq
    deriving (Show, Eq, Ord)

data HMPCState = HMPCState 
    { current_screen    :: Screen
    , screens           :: [Screen]
    , global_keys       :: M.Map Key (IO ())
    }
hmpctest = HMPCState "a" ["a","b","c","d"] (M.singleton Keyx (putStrLn "test"))

data SimpleLayout = SimpleLayout

class Layout l a where
    runLayout :: l -> [a] -> Rectangle -> StateT HMPCState IO ()

instance Layout SimpleLayout Widget where
    runLayout _ xs r = mapM_ (liftIO . putStrLn) xs

type Screen = String 
type Widget = String

wtest :: [Widget]
wtest = ["a","b","c","d"]

processKey :: Key -> StateT HMPCState IO ()
processKey k = do si <- get
                  let a = M.lookup k (global_keys si)
                  liftIO $ maybe (return ()) (liftM id) a

switchScreen :: Int -> StateT HMPCState IO Screen
switchScreen n = do si <- get
                    let ss = screens si
                    let s = ss !! n
                    put (si { screens = ss, current_screen = s})
                    return s

screenAction k = processKey k >> switchScreen 2 >> displayLayout
displayLayout = (runLayout SimpleLayout wtest ) rtest

main = runStateT (screenAction Keyx) hmpctest
