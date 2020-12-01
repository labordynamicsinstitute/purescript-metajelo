{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Arrow ((>>>))
import qualified Data.Map.Strict as DM
import           Data.Maybe (catMaybes)
import           Data.String (IsString(..))
import           Data.String.Interpolate ( i )
import qualified Data.Text as T
import           Path
import           System.Environment (getExecutablePath)
import           Text.XML
import           Text.XML.Cursor
import           ZIO.Prelude
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
  let schemaFile = repoDir </> schemaRelPath
  xsd <- zlift $ readFile def (toFilePath schemaFile)
  let xsdCursor = fromDocument xsd
  let noteCursors = xsdCursor $// element [i|{#{xmlSchema}}documentation|]
  noteEleMap <- pure $ DM.fromList $ noteCursors <&> (\nCurs -> (
      (nCurs & ancestor) <&> (node >>> getEle) & getFirstNodeName
    , nCurs $/ content
    ))
  let allNotes = xsdCursor $// element [i|{#{xmlSchema}}documentation|]  &// content 
  putStrLn $ show allNotes
  putStrLn $ show noteEleMap
  -- putStrLn $ show $ xsd
  where
    getFirstNodeName nds = nds & catMaybes & headMay <&> (elementName >>> nameLocalName)
      & fromMayStr

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

getEle :: Node -> Maybe Element
getEle (NodeElement e) = Just e 
getEle _ = Nothing

fromMayStr :: IsString a => Maybe a -> a
fromMayStr (Just a) = a
fromMayStr Nothing = fromString ""

xmlSchema :: String
xmlSchema = "http://www.w3.org/2001/XMLSchema"