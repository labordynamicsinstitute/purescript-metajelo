module Metajelo.XPaths.Write where

import Prelude (bind, join, map, not, pure, unit, (#), ($), (<>), (>>=))

import Control.Apply                     (lift2)
import Data.Array                        (head, filter)
import Data.Array.NonEmpty               (NonEmptyArray)
import Data.Array.NonEmpty               as NA
import Data.Either                       (Either(..))
import Data.Foldable                     (find)
import Data.Maybe                        (Maybe(..), fromMaybe, isJust)
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
import Web.DOM.Node                      (Node, childNodes, nodeName)
import Web.DOM.NodeList                  (toArray)

-- TODO, remove, for undefined:
import Unsafe.Coerce (unsafeCoerce)
import Prim.TypeError (QuoteLabel, class Warn)
undefined :: forall a. Warn (QuoteLabel "undefined in use") => a
undefined = unsafeCoerce unit

type DocWriter t = t -> Document -> Effect Document

writeRecord :: DocWriter MetajeloRecord
writeRecord rec doc = pure doc >>=
 writeIdentifier rec.identifier >>=
 writeDate rec.date >>=
 writeModDate rec.lastModified >>=
 writeRelIdentifiers rec.relatedIdentifiers >>=
 writeSupplementaryProducts rec.supplementaryProducts

writeIdentifier :: DocWriter Identifier
writeIdentifier = undefined

writeDate :: DocWriter XsdDate
writeDate = undefined

writeModDate :: DocWriter XsdDate
writeModDate = undefined

writeRelIdentifiers :: DocWriter (NonEmptyArray RelatedIdentifier)
writeRelIdentifiers = undefined

writeSupplementaryProducts :: DocWriter (NonEmptyArray SupplementaryProduct)
writeSupplementaryProducts = undefined

