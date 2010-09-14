{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances #-}
             
module Hmpc.Core 
    ( hmpc
    , HMPCState (..)
    ) where

import Hmpc.Layout

import System.Exit
import Control.Monad.State
import qualified Data.Map as M

import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Widgets as W
import Network.MPD as MPD
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

testScreens = testScreena : testScreenb : []
testScreena = Screen $ SimpleScreen testWidgets (Layout StackLayout)
testScreenb = Screen $ SimpleScreen (reverse testWidgets) (Layout StackLayout)

testWidget = W.TextWidget "test" 0 0 W.defaultTWOptions
testWidget2 = W.TextWidget "test2" 0 0 W.defaultTWOptions
testWidgets = map Drawable (testWidget : testWidget2 : [])

exitHmpc = liftIO $ CH.end >> exitSuccess

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

eventloop :: HMPC ()
eventloop = do k <- liftIO $ CH.getKey C.refresh
               processKey k
               eventloop

hmpc = do CH.start
          withMPD $ runStateT (runHMPC eventloop) hmpctest
          CH.end

instance Ord C.Key where
    x < y = show x < show y
    x <= y = show x <= show y
    x >= y = show x >= show y
    x > y = show x > show y
