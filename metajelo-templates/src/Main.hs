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
  let noteEleMap = makeNoteEleMap noteCursors
  -- noteEleMap :: DM.Map T.Text T.Text <- pure $ DM.fromList $ noteCursors <&> (\nCurs -> (
  --     -- (nCurs & ancestor) <&> (node >>> getEle) & getFirstEleName
  --     (nCurs $/ ancestor >=> element [i|{#{xmlSchema}}element|]
  --       &| (node >>> getEle)) & getFirstEleName
  --   , (nCurs $/ content) & T.concat
  --   ))
  let allNotes = xsdCursor $// element [i|{#{xmlSchema}}documentation|]  &// content 
  putStrLn $ show allNotes
  putStrLn $ show noteEleMap
  -- putStrLn $ show $ xsd
  where
    getFirstEleName :: [Maybe Element] -> T.Text
    getFirstEleName els = els & catMaybes & headMay <&> elementAttributes
      >>= (DM.lookup "name") & fromMayStr
    makeNoteEleMap :: [Cursor] -> DM.Map T.Text T.Text
    makeNoteEleMap noteCursors = noteCursors <&> (\nCurs -> (
        (nCurs $/ ancestor >=> element [i|{#{xmlSchema}}element|]
          &| (node >>> getEle)) & getFirstEleName
      , (nCurs $/ content) & T.concat
      )) & DM.fromList & (DM.delete "") <&> T.strip <&> (T.words >>> T.unwords)

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