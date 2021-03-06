{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances, NoMonomorphismRestriction #-}
             
module Hmpc.Core 
    ( hmpc
    , HmpcConfig (..)
    , HMPC 
    , exitHmpc
    , switchScreen
    , Rectangle (..)
    , Layout (..)
    , LayoutClass (..)
    , Screen (..)
    , ScreenClass (..)
    , Drawable (..)
    , DrawableClass (..)
    ) where

import System.Exit
import Control.Exception
import qualified Data.Map as M

import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Widgets as W
import Network.MPD as MPD
import Data.List
import Network.MPD.Core

import Control.Monad.State
import Control.Monad.Error

newtype HMPC a = HMPC { 
                 runHMPC :: StateT HMPCState MPD.MPD a 
                 } deriving (Functor, Monad, MonadIO, MonadState HMPCState, MonadError MPDError, MonadMPD)

instance MonadMPD (StateT HMPCState MPD) where
    open        = lift open
    close       = lift close 
    send        = lift . send
    getPassword = lift getPassword
    receive     = lift receive
    getHandle   = lift getHandle
    setPassword = lift . setPassword
    getVersion  = lift getVersion

data HmpcConfig = HmpcConfig 
    { c_screens       :: [Screen]
    , c_global_keymap :: [(C.Key, (HMPC ()))]
    } 

data HMPCState = HMPCState 
    { hmpc_focus     :: !Int
    , hmpc_screens   :: ![Screen]
    , hmpc_keymap    :: !(M.Map C.Key (HMPC ()))
    }

createHMPCState :: HmpcConfig -> HMPCState
createHMPCState c = HMPCState 0 (c_screens c) (M.fromList $ c_global_keymap c)

data Rectangle = Rectangle
    { rect_x        :: Int
    , rect_y        :: Int
    , rect_width    :: Int
    , rect_height   :: Int 
    } deriving (Show, Eq)

data Layout a = forall l. (LayoutClass l a) => Layout (l a)

class LayoutClass l a where
    runLayout :: l a -> [a] -> Rectangle -> HMPC [a]

instance LayoutClass Layout a where
    runLayout (Layout l) = runLayout l


data Screen = forall a. (ScreenClass a) => Screen a

class ScreenClass a where
    displayScreen :: a -> Rectangle -> HMPC a

instance ScreenClass Screen where
    displayScreen (Screen a) r = do s <- displayScreen a r
                                    return (Screen s)


data Drawable = forall a. (DrawableClass a) => Drawable a

class DrawableClass a where
    draw :: Rectangle -> a -> HMPC a

instance DrawableClass Drawable where
    draw r (Drawable a) = do d <- draw r a
                             return (Drawable d)


exitHmpc :: HMPC ()
exitHmpc = liftIO $ CH.end >> exitSuccess

processKey :: C.Key -> HMPC ()
processKey k = do si <- get
                  let a = M.lookup k (hmpc_keymap si)
                  maybe (return ()) (id) a

switchScreen :: Int -> HMPC ()
switchScreen n = do (h,w) <- liftIO $ C.scrSize
                    ss <- gets hmpc_screens 
                    let rect = Rectangle 0 0 (w-1) (h-1)
                    s' <- displayScreen (ss !! n) rect
                    let ss' = replaceNth n s' ss
                    modify (\s -> s { hmpc_screens = ss', hmpc_focus = n})

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x xs = h ++ [x] ++ (tail t)
                  where (h,t) = splitAt n xs

eventloop :: HMPC ()
eventloop = do k <- liftIO $ CH.getKey C.refresh
               processKey k
               eventloop

hmpc :: HmpcConfig -> IO ()
hmpc c = do let c' = createHMPCState c 
            CH.start
            _ <- withMPD $ runStateT (runHMPC eventloop) c'
            CH.end

instance Ord C.Key where
    x < y = show x < show y
    x <= y = show x <= show y
    x >= y = show x >= show y
    x > y = show x > show y
