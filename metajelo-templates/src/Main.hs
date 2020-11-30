{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           ZIO.Prelude

import           Data.String.Interpolate ( i )
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
  let schemaFile = repoDir </> schemaRelPath
  xsd <- zlift $ readFile def (toFilePath schemaFile)
  let xsdCursor = fromDocument xsd
  --let noteCursors = xsdCursor $// element "documentation"
  let allNotes = xsdCursor $// element [i|{#{xmlSchema}}documentation|]  &// content 
  putStrLn $ show repoDir
  putStrLn $ show allNotes
  -- putStrLn $ show $ xsd

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

schemaRelPath :: Path Rel File
schemaRelPath = [relfile|schema/metajelo.xsd|]


xmlSchema :: String
xmlSchema = "http://www.w3.org/2001/XMLSchema"