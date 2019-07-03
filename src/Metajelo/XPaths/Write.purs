module Metajelo.XPaths.Write where

import Prelude (class Show, Unit, bind, discard, join, map, not, pure, show,
unit, (#), ($), (<>), (>>=), (<#>))

import Control.Apply                     (lift2)
import Data.Array                        (head, filter)
import Data.Array.NonEmpty               (NonEmptyArray)
import Data.Array.NonEmpty               as NA
import Data.Either                       (Either(..))
import Data.Foldable                     (find, for_)
import Data.Maybe                        (Maybe(..), fromMaybe, isJust)
--import Data.Show                         (class Show)
import Data.String.Utils                 (startsWith)
import Data.Traversable                  (sequence)
import Data.XPath                        (class XPathLike, root, at, xx, (/?), (//))
import Effect                            (Effect)
import Effect.Exception                  (throw)

import Metajelo.Types
import Metajelo.XPaths

import Text.Email.Validate               (validate)
import URL.Validator                     (URL)
import URL.Validator                     as URL
import Web.DOM.Document                  (Document, createElementNS, getElementsByTagName,
                                          getElementsByTagNameNS)
import Web.DOM.DOMParser                 (makeDOMParser, parseXMLFromString)
import Web.DOM.Document.XPath            (NSResolver)
import Web.DOM.Document.XPath            as XP
import Web.DOM.Document.XPath.ResultType as RT
import Web.DOM.Element                   (Element, fromNode, getAttribute
                                         , localName, prefix, setAttribute, toNode)
import Web.DOM.HTMLCollection            (item)
import Web.DOM.Node                      (Node, appendChild, childNodes
                                         , nodeName, setNodeValue
                                         , setTextContent)
import Web.DOM.NodeList                  (toArray)

-- TODO, remove, for undefined:
import Unsafe.Coerce (unsafeCoerce)
import Prim.TypeError (QuoteLabel, class Warn)
undefined :: forall a. Warn (QuoteLabel "undefined in use") => a
undefined = unsafeCoerce unit

type DocWriterRoot t = ParseEnv -> t -> Effect Unit
type DocWriter t = ParseEnv -> Node -> t -> Effect Unit

-- | For convenience, this method assumes that a document with
-- | the outer layers "above" individual supplementary producs
-- | exist in the `env` environment (sans perhaps a few attributes which
-- | will be filled in). This means the starting doc (`blankDoc`) used
-- | is not valid Metajelo, but successfully running writeRecord on it
-- | will return a valid Metajelo document.
writeRecord :: DocWriterRoot MetajeloRecord
writeRecord env rec = do
 writeIdentifier env rec.identifier
 writeDate env rec.date
 writeModDate env rec.lastModified
 writeRelIdentifiers env rec.relatedIdentifiers
 writeSupplementaryProducts env rec.supplementaryProducts

writeIdentifier :: DocWriterRoot Identifier
writeIdentifier env recId = do
  idNd <- unsafeSingleNodeValue env env.recNode (xx idP)
  writeIdContents idTypeAT env idNd recId

writeResourceID :: DocWriter ResourceID
writeResourceID env prodNd resId = do
  resIdNd <- map toNode $ createAppendRecEle env prodNd resIdP
  -- TODO: currently ResourceID uses a different attribute name
  --     : which makes writeIdContents/ more complicated
  writeIdContents resIdTypeAT env resIdNd resId

writeInstitutionID :: DocWriter InstitutionID
writeInstitutionID env prodNd instId = do
  instIdNd <- map toNode $ createAppendRecEle env prodNd instIdP
  writeIdContents idTypeAT env instIdNd instId

writeIdContents :: String -> DocWriter Identifier
writeIdContents atName env parNode id = do
  setTextContent id.id parNode
  writeIdentifierType atName env parNode id.idType

writeIdentifierType :: String -> DocWriter IdentifierType
writeIdentifierType atName env idNode idType = do
  let idElMay = fromNode idNode
  _ <- sequence $ idElMay
    <#> (\idEl -> setAttribute atName (show idType) idEl)
  pure unit

writeDate :: DocWriterRoot XsdDate
writeDate env date = do
  dateNdMay <- env.xevalRoot.nodeMay dateRootP
  writeNodeMay date dateNdMay

writeModDate :: DocWriterRoot XsdDate
writeModDate env date = do
  dateNdMay <- env.xevalRoot.nodeMay lastModRootP
  writeNodeMay date dateNdMay

writeRelIdentifiers :: DocWriterRoot (NonEmptyArray RelatedIdentifier)
writeRelIdentifiers env relIds = for_ relIds (\relId -> do
  el <- createAppendRecEle env env.recNode relIdP
  let nd = toNode el
  setTextContent relId.id nd
  setAttribute relIdTypeAT (show relId.idType) el
  setAttribute relTypeAT (show relId.relType) el
)

writeSupplementaryProducts :: DocWriterRoot (NonEmptyArray SupplementaryProduct)
writeSupplementaryProducts env prods = for_ prods (\p -> writeProduct env p)

writeProduct :: DocWriterRoot SupplementaryProduct
writeProduct env prod = do
  prodContainer <- unsafeSingleNodeValue env env.recNode sProdCP
  prodNd <- map toNode $ createAppendRecEle env env.recNode sProdP
  writeBasicMetadata env prodNd prod.basicMetadata
  _ <- sequence $ prod.resourceID <#> (\resId -> writeResourceID env prodNd resId)
  writeResourceType env prodNd prod.resourceType
  writeFormats env prodNd prod.format
  writeResourceMetadataSource env prodNd prod.resourceMetadataSource
  writeLocation env prodNd prod.location

writeBasicMetadata :: DocWriter BasicMetadata
writeBasicMetadata env prodNd bm = do
  bmNd <- map toNode $ createAppendRecEle env prodNd basicMetaP
  titleNd <- map toNode $ createAppendRecEle env bmNd titleP
  setTextContent bm.title titleNd
  creatorNd <- map toNode $ createAppendRecEle env bmNd creatorP
  setTextContent bm.creator creatorNd
  pubYearNd <- map toNode $ createAppendRecEle env bmNd pubYearP
  setTextContent bm.publicationYear pubYearNd

writeResourceType :: DocWriter ResourceType
writeResourceType env prodNd resType = do
  resTypeEl <- createAppendRecEle env prodNd resTypeP
  setTextContent resType.description $ toNode resTypeEl
  setAttribute resTypeGenAT (show resType.generalType) resTypeEl

writeFormats :: DocWriter (Array Format)
writeFormats env prodNd formats = do
  fContNd <- map toNode $ createAppendRecEle env prodNd formatCP
  for_ formats (\f -> writeFormat env fContNd f)

writeFormat :: DocWriter Format
writeFormat env fContNd format = do
  formEl <- createAppendRecEle env fContNd formatP
  setTextContent format $ toNode formEl

writeResourceMetadataSource :: DocWriter (Maybe ResourceMetadataSource)
writeResourceMetadataSource env prodNd resMdSources = undefined

writeLocation :: DocWriter Location
writeLocation env prodNd loc = undefined


----- Utility functions below -----

writeNodeMay :: String -> Maybe Node -> Effect Unit
writeNodeMay str ndMay = do
  _ <- sequence $ map (setTextContent str) ndMay
  pure unit

createAppendRecEle :: ParseEnv -> Node -> String -> Effect Element
createAppendRecEle env parNode tag = do
  el <- createRecEle env tag
  _ <- appendChild (toNode el) parNode
  pure el

-- | Less safe than using `createAppendRecEle` directly,
-- | due to the possibility of creating dangling nodes.
createRecEle :: ParseEnv -> String -> Effect Element
createRecEle env tag = do
  let recPfxMay = prefix env.recElem
  recPfx <- pure $ case recPfxMay of
    Just pfx -> pfx <> ":"
    Nothing -> ""
  let tagName = recPfx <> tag
  createElementNS (Just env.ns) tagName env.doc

blankDoc :: String
blankDoc = """TODO
"""
