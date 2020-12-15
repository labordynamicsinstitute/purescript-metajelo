{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Arrow ((>>>))
import qualified Data.Map.Strict as DM
-- import qualified Data.Set as DS
import           Data.Maybe (catMaybes)
import           Data.String (IsString(..))
import           Data.String.Interpolate ( i, iii, __i )
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  repoDir <- getRepoDir
  putStrLn $ show repoDir
  let schemaFile = repoDir </> schemaRelPath
  xsd <- zlift $ readFile def (toFilePath schemaFile)
  let xsdCursor = fromDocument xsd
  let noteCursors = xsdCursor $// element [i|{#{xmlSchema}}documentation|]
  let noteEleMap = makeNoteMap "element" noteCursors
  let noteAttrMap = makeNoteMap "attribute" noteCursors
  let noteCplxTypeMap = makeNoteMap "complexType" noteCursors
  let noteSimpTypeMap = makeNoteMap "simpleType" noteCursors
  allDescrMaps <- pure $ DM.fromList [
      (eleSfx, noteEleMap)
    , (attSfx, noteAttrMap)
    , (cTypeSfx, noteCplxTypeMap)
    , (sTypeSfx, noteSimpTypeMap)
    ]

  -- simpleType can be annotated directly
  -- element belongs to complexType; the convetion is the element is a record field
  --   with name used as-is, exept the first character may be lower-cased in PureScript
  --   if the name is upper case in the XSD
  -- attribute is the same as element in this regard

  -- In order to keep things relatively simple, we can build up a record,
  -- where the keys are fully-qualified names
  -- that line up with the record-type names, e.g. "someComplexType.someElement.someAttribute"
  -- This means, for each of the elements and attributes above, we also need a Haskell Map
  -- that allows us to look up the "owner" type. The PureScript record will be shapped statically,
  -- and we can implement a counter so that we verify we've used all of the found Haskell
  -- documentation strings.

  writeSchemaInfoFile repoDir allDescrMaps

  -- Tests follow
  let allNotes = xsdCursor $// element [i|{#{xmlSchema}}documentation|]  &// content
  let allMapsTest = DM.toList allDescrMaps
  let allKeysTest = join $ (\(s,m) -> (\k -> k <> s) <$> DM.keys m) <$> allMapsTest
  zlift $ T.putStrLn $ T.intercalate "\n\n" allNotes
  -- let allDescrKeys = DS.unions $ DS.fromList <$> (DM.elems $ DM.keys <$> allDescrMaps)
  assertTrue [iii|length allNotes == length allKeysTest ::
                #{length allNotes} == #{length allKeysTest}|]
    (length allNotes == length allKeysTest)
  where
    getFirstEleName :: [Maybe Element] -> T.Text
    getFirstEleName els = els & catMaybes & headMay <&> elementAttributes
      >>= (DM.lookup "name") & fromMayStr
    makeNoteMap :: String -> [Cursor] -> DM.Map T.Text T.Text
    makeNoteMap eTag noteCursors = noteCursors <&> (\nCurs -> (
        (nCurs $/ ancestor >=> element [i|{#{xmlSchema}}#{eTag}|]
          &| (node >>> getEle)) & getFirstEleName
      , (nCurs $/ content) & T.concat
      )) & DM.fromList & (DM.delete "") <&> T.strip <&> (T.words >>> T.unwords)


writeSchemaInfoFile :: Path Abs Dir -> DM.Map T.Text (DM.Map T.Text T.Text)
  -> AppEnv ()
writeSchemaInfoFile repoDir allMapsMap =
  zlift $ T.writeFile (toFilePath outFile) outTxt
  where
    allMaps :: [(T.Text, DM.Map T.Text T.Text)]
    allMaps = DM.toList allMapsMap
    allKeys = (\(s,m) -> (s, DM.keys m)) <$> allMaps
    outFile = repoDir </> metajeloSrcDir </> infoFile
    descrVarTxt = T.intercalate "\n\n" $ join $ (uncurry descrEntries) <$> allMaps
    descrMapTxt = T.intercalate "\n\n" $ (uncurry makeDescrMap) <$> allKeys
    outTxt = descrHeader <> "\n\n"
             <> "\n\n" <> descrVarTxt
             <> "\n\n" <> descrMapTxt
             <> "\n" -- newline at EOF

dscrSfx :: T.Text
dscrSfx = "Dscr"

eleSfx :: T.Text
eleSfx = "Ele"

attSfx :: T.Text
attSfx = "Attr"

cTypeSfx :: T.Text
cTypeSfx = "CTyp"

sTypeSfx :: T.Text
sTypeSfx = "STyp"

-- allSfx :: T.Text
-- allSfx = "All"

makeDescrMap :: T.Text -> [T.Text] -> T.Text
makeDescrMap sfx dKeys = dsHeader <> "\n  " <> dsEntries <>  "\n}"
  where
    dsHeader = [__i|descr#{sfx}Map :: FO.Object String
                    descr#{sfx}Map = FO.fromHomogeneous {|]
    dsEntry :: T.Text -> T.Text
    dsEntry k = [__i|#{k}#{sfx}: #{k}#{sfx}#{dscrSfx}|]
    dsEntries = T.intercalate "\n, " (dsEntry <$> dKeys)

descrEntries :: T.Text -> DM.Map T.Text T.Text -> [T.Text]
descrEntries sfx descrMap = makeDescrEntry sfx <$> DM.toList descrMap
--
makeDescrEntry :: T.Text -> (T.Text, T.Text) -> T.Text
makeDescrEntry sfx kv = [__i|#{fst kv}#{sfx}#{dscrSfx} :: String
                         #{fst kv}#{sfx}#{dscrSfx} = "#{snd kv}"|]

--
descrHeader :: T.Text
descrHeader =
  [__i|-- | This module contains additional information about
       -- | the Metajelo Schema.
       module Metajelo.#{infoModule} where

       import Foreign.Object as FO
       |]


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

metajeloSrcDir :: Path Rel Dir
metajeloSrcDir = [reldir|src/Metajelo|]

infoFile :: Path Rel File
infoFile = [relfile|SchemaInfo.purs|]
--
infoModule :: String
infoModule = takeWhile (/= '.') $ toFilePath infoFile

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

assertTrue :: String -> Bool -> AppEnv ()
assertTrue _ True = pure ()
assertTrue msg False = throwString $ "Failed: " <> msg
