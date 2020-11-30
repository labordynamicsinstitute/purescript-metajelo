{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Makeshift prelude until zio-prelude is in place -} 
module ZIO.Prelude (
  module ZIO.Prelude
, (.)
, ($)
, Bool(..)
, IO
, Show(..)
, String
, fst
, snd
) where

import qualified Control.Exception.Safe as SX
import           Path
import qualified Prelude as P
import           Prelude ((.), ($), Bool(..), IO, Show(..), String, fst, snd)

import           ZIO.Trans

putStrLn :: String -> ZIO r SomeNonPseudoException () 
putStrLn = zlift . P.putStrLn

throwString :: forall r a. String -> ZIO r SomeNonPseudoException a
throwString = zlift . SX.throwString

-- TODO: classy variant?
putStrLnIO :: String -> IO ()
putStrLnIO = P.putStrLn
