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
import Control.Monad.State
import qualified Data.Map as M

import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Widgets as W
import Network.MPD as MPD
import Network.MPD.Core

import Control.Monad.State()
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
    { current_screen    :: Screen
    , screens           :: [Screen]
    , global_keymap     :: M.Map C.Key (HMPC ())
    }

createHMPCState :: HmpcConfig -> HMPCState
createHMPCState c = HMPCState (head $ c_screens c) (c_screens c) (M.fromList $ c_global_keymap c)

data Rectangle = Rectangle
    { rect_x        :: Int
    , rect_y        :: Int
    , rect_width    :: Int
    , rect_height   :: Int 
    } deriving (Show, Eq)

data Layout a = forall l. (LayoutClass l a) => Layout (l a)

class LayoutClass l a where
    runLayout :: l a -> [a] -> Rectangle -> HMPC ()

instance LayoutClass Layout a where
    runLayout (Layout l) = runLayout l


data Screen = forall a. (ScreenClass a) => Screen a

class ScreenClass a where
    displayScreen :: a -> Rectangle -> HMPC ()

instance ScreenClass Screen where
    displayScreen (Screen a) r = displayScreen a r


data Drawable = forall a. (DrawableClass a) => Drawable a

class DrawableClass a where
    draw :: Rectangle -> a -> HMPC ()

instance DrawableClass Drawable where
    draw r (Drawable a) = draw r a


exitHmpc :: HMPC ()
exitHmpc = liftIO $ CH.end >> exitSuccess

processKey :: C.Key -> HMPC ()
processKey k = do si <- get
                  let a = M.lookup k (global_keymap si)
                  maybe (return ()) (id) a

switchScreen :: Int -> HMPC ()
switchScreen n = do si <- get
                    let ss = screens si
                    let s = ss !! n
                    put (si { screens = ss, current_screen = s})
                    (h,w) <- liftIO $ C.scrSize
                    let rect = Rectangle 0 0 (w-1) (h-1)
                    displayScreen s rect

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
