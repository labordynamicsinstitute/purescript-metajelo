module Metajelo.XPaths where

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


recP :: String
recP = "record"

idP :: String
idP = "identifier"

relIdP :: String
relIdP = "relatedIdentifier"

dateP :: String
dateP = "date"

lastModP :: String
lastModP = "lastModified"

instIdP :: String
instIdP = "institutionID"

instNameP :: String
instNameP = "institutionName"

instTypeP :: String
instTypeP = "institutionType"

instContactP :: String
instContactP = "institutionContact"

instSustainP :: String
instSustainP = "institutionSustainability"

instPoliciesP :: String
instPoliciesP = "institutionPolicies"

instPolicyP :: String
instPolicyP = "institutionPolicy"

versioningP :: String
versioningP = "versioning"

locP :: String
locP = "location"

superOrgNameP :: String
superOrgNameP = "superOrganizationName"

missionUrlP :: String
missionUrlP = "missionStatementURL"

fundingUrlP :: String
fundingUrlP = "fundingStatementURL"

basicMetaP :: String
basicMetaP = "basicMetadata"

titleP :: String
titleP = "Title"

creatorP :: String
creatorP = "Creator"

pubYearP :: String
pubYearP = "PublicationYear"

resIdP :: String
resIdP = "resourceID"

resMetaSourceP :: String
resMetaSourceP = "resourceMetadataSource"

idTypeAP :: String
idTypeAP = "@identifierType"

relIdTypeAP :: String
relIdTypeAP  = "@relatedIdentifierType"

relTypeAP :: String
relTypeAP = "@relationType"

instContactTypeAP :: String
instContactTypeAP  = "@institutionContactType"

polTypeAP :: String
polTypeAP = "@policyType"

appliesToProdAP :: String
appliesToProdAP = "@appliesToProduct"

idTypeRootAP :: String
idTypeRootAP = idRootP // idTypeAP

recRootP :: String
recRootP = root /? recP

idRootP :: String
idRootP = recRootP /? idP

dateRootP :: String
dateRootP = recRootP /? dateP

lastModRootP :: String
lastModRootP = recRootP /? lastModP

relIdRootP :: String
relIdRootP = recRootP /? relIdP

sProdRootP :: String
sProdRootP = recRootP /? "supplementaryProducts" /? "supplementaryProduct"



metajeloNamespaces :: NonEmptyArray String
metajeloNamespaces = NA.cons' "http://ourdomain.cornell.edu/reuse/v.01" []

-- | The current Metajelo namespace URL, provided as a fallback
defaultMetajeloNS :: String
defaultMetajeloNS = "http://ourdomain.cornell.edu/reuse/v.01"

-- | Resolver that returns `defaultMetajeloNS` as a fallback
getMetajeloResolver :: Node -> Document -> Effect NSResolver
getMetajeloResolver node doc = do
  nsResolver <- XP.defaultNSResolver node doc
  -- traceM nsResolver
  nodeEleMay :: Maybe Element <- pure $ fromNode node
  defaultNS :: String <- getDefaultNS nodeEleMay
  pure $ XP.customNSResolver $ makeMjNSResFun nsResolver defaultNS
  where
    getDefaultNS :: Maybe Element -> Effect String
    getDefaultNS mayElem = do
      case mayElem of
        Nothing -> pure $ defaultMetajeloNS
        Just elem -> map nsOrGuess (getAttribute "xmlns" elem)
    nsOrGuess :: Maybe String -> String
    nsOrGuess nsMay = fromMaybe defaultMetajeloNS nsMay
    makeMjNSResFun :: NSResolver -> String -> String -> String
    makeMjNSResFun nsr defNS prefix = case XP.lookupNamespaceURI nsr prefix of
      Nothing -> defNS
      Just ns -> ns

type ParseEnv = {
  doc :: Document
, recNode :: Node
, xeval :: MJXpathEvals
, xevalRoot :: MJXpathRootEvals
}

type MJXpathEvals = {
    any     :: Node -> String -> RT.ResultType -> Effect XP.XPathResult
  , num     :: Node -> String -> Effect Number
  , str     :: Node -> String -> Effect String
  , bool    :: Node -> String -> Effect Boolean
  , nodeMay :: Node -> String -> Effect (Maybe Node)
}

type MJXpathRootEvals = {
    any     :: String -> RT.ResultType -> Effect XP.XPathResult
  , num     :: String -> Effect Number
  , str     :: String -> Effect String
  , bool    :: String -> Effect Boolean
  , nodeMay :: String -> Effect (Maybe Node)
}

mkMetajeloXpathEval :: Document -> Maybe NSResolver -> MJXpathEvals
mkMetajeloXpathEval doc nsResMay = {
    any     : (\n x r -> XP.evaluate x n nsResMay r Nothing doc)
  , num     : (\n x -> XP.evaluateNumber x n nsResMay Nothing doc)
  , str     : (\n x -> XP.evaluateString x n nsResMay Nothing doc)
  , bool    : (\n x -> XP.evaluateBoolean x n nsResMay Nothing doc)
  , nodeMay : (\n x -> XP.evaluate x n nsResMay RT.any_unordered_node_type Nothing doc
                       >>= XP.singleNodeValue)
}

getDefaultParseEnv :: String -> Effect ParseEnv
getDefaultParseEnv xmlDocStr = do
  dp <- makeDOMParser
  recDocEi <- parseXMLFromString xmlDocStr dp
  recDoc <- case recDocEi of
    Left er -> throw $ "XML parsing error: " <> er
    Right doc -> pure doc
  recNodeMay <- recordOfDoc recDoc
  recNode <- case recNodeMay of
    Nothing -> throw "Could not find <record> element!"
    Just nd -> pure nd
  nsRes <- getMetajeloResolver recNode recDoc
  defEvals <- pure $ mkMetajeloXpathEval recDoc (Just nsRes)
  pure $ {
      doc: recDoc
    , recNode: recNode
    , xeval : defEvals
    , xevalRoot : {
        any : defEvals.any recNode
      , num : defEvals.num recNode
      , str : defEvals.str recNode
      , bool : defEvals.bool recNode
      , nodeMay : defEvals.nodeMay recNode
    }
  }

recordOfDoc :: Document -> Effect (Maybe Node)
recordOfDoc doc = do
  recCollection <- getElementsByTagName recP doc
  recordMayNoNS <- item 0 recCollection
  recordMay <- case recordMayNoNS of
    Nothing -> do
      maybeRecs <- sequence $ map getRecByNS metajeloNamespaces
      pure $ join $ find isJust maybeRecs
    Just recMay -> pure $ Just recMay
  pure $ map Ele.toNode recordMay
  where
    getRecByNS ns = do
      recCol <- getElementsByTagNameNS (Just ns) recP doc
      item 0 recCol
