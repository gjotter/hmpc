{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances #-}
             
module Hmpc.Core 
    ( 
    ) where

import Network.MPD as MPD
import Network.MPD.Core

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M

data HMPCState = HMPCState 
    { current_screen    :: String
    , screens           :: [String]
    , global_keys       :: M.Map Key (HMPC ())
    }

hmpctest = HMPCState "a" ["a","b","c"] (M.fromList keyList)

newtype HMPC a = HMPC { 
                 runHMPC :: StateT HMPCState MPD.MPD a 
                 } deriving (Functor, Monad, MonadIO, MonadState HMPCState, MonadError MPDError, MonadMPD)

instance MonadMPD (StateT HMPCState MPD) where
    open = lift open
    close = lift close 
    send = lift . send
    getPassword = lift getPassword
    receive = lift receive
    getHandle = lift getHandle
    setPassword = lift . setPassword
    getVersion = lift getVersion
    

data Key = Keyj | Keyk
           deriving (Eq, Ord, Show)

keyList = 
    [(Keyj,    switchScreen 0)
    ,(Keyk,    switchScreen 1)
    ] 

processKey :: Key -> HMPC ()
processKey k = do si <- get
                  let a = M.lookup k (global_keys si)
                  maybe (return ()) (id) a

switchScreen :: Int -> HMPC ()
switchScreen n = do si <- get
                    let ss = screens si
                    let s = ss !! n
                    o <- play Nothing >> currentSong
                    liftIO $ maybe (return ()) (putStrLn . show) o
                    put (si { screens = ss, current_screen = s})


liftPlay :: (MonadTrans t, MonadMPD m) => Maybe Int -> t m ()
liftPlay = lift . play


