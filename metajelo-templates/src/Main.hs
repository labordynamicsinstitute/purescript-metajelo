{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           ZIO.Prelude

import           Path
import           ZIO.Trans


main :: IO ()
main = do
  runApp app ()
  where
    runApp a r = runZIO a r (putStrLnIO . show)

type Env = ()
type AppEnv a = ZIO Env SomeNonPseudoException a

app :: AppEnv ()
app = do
  putStrLn "hello from ZIO"
