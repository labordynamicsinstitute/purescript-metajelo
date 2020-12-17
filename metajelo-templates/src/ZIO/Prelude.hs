{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Makeshift prelude until zio-prelude is in place -} 
module ZIO.Prelude (
  module ZIO.Prelude
, (.)
, (<>)
, ($)
, (&)
, (<$>)
, (<&>)
, (==)
, (/=)
, Bool(..)
, Maybe(..)
, IO
, Applicative(..)
, Foldable(..)
, Show(..)
, String
, const
, filter, takeWhile
, flip
, fst, snd
, null
, sum
, and, or, not
, uncurry
) where

import qualified Control.Exception.Safe as SX
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Path
import qualified Prelude as P
import           Prelude ((.), (<>), ($), (<$>), (==), (/=), Bool(..), Maybe(..), IO, Applicative(..), Foldable(..), Show(..), String, const, filter, takeWhile, flip, fst, snd, null, sum, and, or, not, uncurry)
import           System.Exit as SE
import           System.IO.Unsafe (unsafePerformIO)
import           ZIO.Trans

putStrLn :: String -> ZIO r SomeNonPseudoException () 
putStrLn = zlift . P.putStrLn

throwString :: forall r a. String -> ZIO r SomeNonPseudoException a
throwString = zlift . SX.throwString

-- TODO: classy variant?
putStrLnIO :: String -> IO ()
putStrLnIO = P.putStrLn


 -- -- From safe, RIO, etc:
 -- liftMay :: (a -> b) -> (a -> Bool) -> (a -> Maybe b)
 -- liftMay func test val = if test val then Nothing else Just $ func val
 -- --
 -- headMay, lastMay :: [a] -> Maybe a
 -- headMay = liftMay null P.head
 -- lastMay = liftMay null P.last

headMay :: Foldable f => f a -> Maybe a
headMay = foldr (const . Just) Nothing

-- Attempt to handle any exception.
-- Not entirely sure how "safe" this is
-- TODO: add to ZIO
mapZErrorOrExit :: ZIO r  SX.SomeException a -> ZIO r SomeNonPseudoException a
mapZErrorOrExit m = mapZError mapZE m
  -- (e -> SomeNonPseudoException) 
  where
    mapZE ::  SX.SomeException -> SomeNonPseudoException
    mapZE e = case SX.fromException e of
      Just se -> se
      Nothing ->
        unsafePerformIO $ SE.die  "Received a PseudoException, exiting! "
