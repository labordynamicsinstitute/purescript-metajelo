{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           ZIO.Prelude

import qualified Data.Text as T
import           Path
import           System.Environment (getExecutablePath)
import           Text.XML
import           Text.XML.Cursor
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
  repoDir <- getRepoDir
  putStrLn $ show repoDir

getRepoDir :: AppEnv (Path Abs Dir)
getRepoDir = do
  ePath <- zlift $ getExecutablePath
  let ePathT = T.pack ePath
  if
    | stackIn `T.isInfixOf` ePathT -> breakOnDir stackIn ePathT
    | cabalIn `T.isInfixOf` ePathT -> breakOnDir cabalIn ePathT
    | True -> throwString "Couldn't determine project exe path"
  where
    stackIn = "metajelo-templates/.stack-work"
    cabalIn = "metajelo-templates/dist-newstyle"
    breakOnDir bDir ePath = zlift $ parseAbsDir $ T.unpack $ fst $ T.breakOn
      bDir ePath
