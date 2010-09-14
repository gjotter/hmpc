{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances #-}
             
module Hmpc.Core 
    ( hmpc
    ) where

import Network.MPD as MPD

import Control.Monad.State
import qualified Data.Map as M

data HMPCState = HMPCState 
    { current_screen    :: String
    , screens           :: [String]
    , global_keys       :: M.Map Key (HMPC2 ())
    }

hmpctest = HMPCState "a" ["a","b","c"] (M.fromList keyList)

type HMPC = StateT HMPCState IO 
type HMPC2 = StateT HMPCState MPD.MPD

data Key = Keyj | Keyk
           deriving (Eq, Ord, Show)

keyList = 
    [(Keyj,    switchScreen 0)
    ,(Keyk,    switchScreen 1)
    ] 

processKey :: Key -> HMPC2 ()
processKey k = do si <- get
                  let a = M.lookup k (global_keys si)
                  maybe (return ()) (id) a

switchScreen :: Int -> HMPC2 ()
switchScreen n = do si <- get
                    let ss = screens si
                    let s = ss !! n
                    put (si { screens = ss, current_screen = s})

printArtist :: HMPC2 ()
printArtist = liftIO $ putStrLn ",,,"      
