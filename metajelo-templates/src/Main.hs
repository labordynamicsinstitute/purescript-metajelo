{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import           Prelude as P

import           Path
import           ZIO.Trans


main :: IO ()
main = do
  runApp app ()
  where
    runApp a r = runZIO a r (putStrLn . show)

type Env = ()
type AppEnv a = ZIO Env SomeNonPseudoException a

app :: AppEnv ()
app = do
  println "hello from ZIO"

println :: String -> ZIO r SomeNonPseudoException ()
println x = zlift $ P.putStrLn x