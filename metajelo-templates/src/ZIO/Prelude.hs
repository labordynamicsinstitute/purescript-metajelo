{-# LANGUAGE NoImplicitPrelude #-}

{- | Makeshift prelude until zio-prelude is in place -} 
module ZIO.Prelude (
  module ZIO.Prelude
, (.)
, ($)
, IO(..)
, Show(..)
, String
) where

import qualified Prelude as P
import           Prelude ((.), ($), IO(..), Show(..), String)

import           ZIO.Trans

putStrLn :: String -> ZIO r SomeNonPseudoException () 
putStrLn x = zlift $ P.putStrLn x

-- TODO: classy variant?
putStrLnIO :: String -> IO ()
putStrLnIO = P.putStrLn