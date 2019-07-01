module Metajelo.XPaths.Write where

import Prelude (class Show, Unit, bind, discard, join, map, not, pure, show,
unit, (#), ($), (<>), (>>=))

import Control.Apply                     (lift2)
import Data.Array                        (head, filter)
import Data.Array.NonEmpty               (NonEmptyArray)
import Data.Array.NonEmpty               as NA
import Data.Either                       (Either(..))
import Data.Foldable                     (find)
import Data.Maybe                        (Maybe(..), fromMaybe, isJust)
--import Data.Show                         (class Show)
import Data.String.Utils                 (startsWith)
import Data.Traversable                  (sequence)
import Data.XPath                        (class XPathLike, root, xx, (/?), (//))
import Effect                            (Effect)
import Effect.Exception                  (throw)

import Metajelo.Types
import Metajelo.XPaths

import Text.Email.Validate               (validate)
import URL.Validator                     (URL)
import URL.Validator                     as URL
import Web.DOM.Document                  (Document, getElementsByTagName,
                                          getElementsByTagNameNS)
import Web.DOM.DOMParser                 (makeDOMParser, parseXMLFromString)
import Web.DOM.Document.XPath            (NSResolver)
import Web.DOM.Document.XPath            as XP
import Web.DOM.Document.XPath.ResultType as RT
import Web.DOM.Element                   (Element, fromNode, getAttribute, localName)
import Web.DOM.Element                   as Ele
import Web.DOM.HTMLCollection            (item)
import Web.DOM.Node                      (Node, childNodes, nodeName, setNodeValue
                                         , setTextContent)
import Web.DOM.NodeList                  (toArray)

-- TODO, remove, for undefined:
import Unsafe.Coerce (unsafeCoerce)
import Prim.TypeError (QuoteLabel, class Warn)
undefined :: forall a. Warn (QuoteLabel "undefined in use") => a
undefined = unsafeCoerce unit

type DocWriterRoot t = ParseEnv -> t -> Effect Unit
type DocWriter t = ParseEnv -> t ->  Node -> Effect Unit

writeRecord :: DocWriterRoot MetajeloRecord
writeRecord env rec = do
 writeIdentifier env rec.identifier
 writeDate env rec.date
 writeModDate env rec.lastModified
 writeRelIdentifiers env rec.relatedIdentifiers
 writeSupplementaryProducts env rec.supplementaryProducts

writeIdentifier :: DocWriterRoot Identifier
writeIdentifier env recId = do
  recNdMay <- env.xevalRoot.nodeMay idRootP
  writeNodeMay recId.id recNdMay
  writeIdentifierType env recId.idType

writeIdentifierType :: DocWriterRoot IdentifierType
writeIdentifierType env idType = do
  idTypeNdMay <- env.xevalRoot.nodeMay idTypeRootAP
  writeNodeMay (show idType) idTypeNdMay

writeDate :: DocWriterRoot XsdDate
writeDate env date = do
  dateNdMay <- env.xevalRoot.nodeMay dateRootP
  writeNodeMay date dateNdMay

writeModDate :: DocWriterRoot XsdDate
writeModDate env date = do
  dateNdMay <- env.xevalRoot.nodeMay lastModRootP
  writeNodeMay date dateNdMay

writeRelIdentifiers :: DocWriterRoot (NonEmptyArray RelatedIdentifier)
writeRelIdentifiers = undefined

writeSupplementaryProducts :: DocWriterRoot (NonEmptyArray SupplementaryProduct)
writeSupplementaryProducts = undefined

writeNodeMay :: String -> Maybe Node -> Effect Unit
writeNodeMay str ndMay = do
  _ <- sequence $ map (setTextContent str) ndMay
  pure unit
