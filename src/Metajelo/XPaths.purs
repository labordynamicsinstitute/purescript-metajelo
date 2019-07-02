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
import Data.XPath                        (class XPathLike, root, at, xx, (/?), (//))
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

-- Naming conventions: AT = attribute, AP = attribute path
--                   , P = (element) Path

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

idTypeAT :: String
idTypeAT = "identifierType"

relIdTypeAT :: String
relIdTypeAT  = "relatedIdentifierType"

relTypeAT :: String
relTypeAT = "relationType"

instContactTypeAT :: String
instContactTypeAT  = "institutionContactType"

polTypeAT :: String
polTypeAT = "policyType"

appliesToProdAT :: String
appliesToProdAT = "appliesToProduct"

idTypeRootAP :: String
idTypeRootAP = idRootP // at idTypeAT

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

-- | Determine
getDefaultNS :: Maybe Element -> Effect String
getDefaultNS mayElem = do
  case mayElem of
    Nothing -> pure $ defaultMetajeloNS
    Just elem -> map nsOrGuess (getAttribute "xmlns" elem)
  where
    nsOrGuess :: Maybe String -> String
    nsOrGuess nsMay = fromMaybe defaultMetajeloNS nsMay

-- | Resolver that returns `defaultMetajeloNS` as a fallback
getMetajeloResolver :: Node -> Document -> Effect NSResolver
getMetajeloResolver node doc = do
  nsResolver <- XP.defaultNSResolver node doc
  -- traceM nsResolver
  nodeEleMay :: Maybe Element <- pure $ fromNode node
  defaultNS :: String <- getDefaultNS nodeEleMay
  pure $ XP.customNSResolver $ makeMjNSResFun nsResolver defaultNS
  where
    makeMjNSResFun :: NSResolver -> String -> String -> String
    makeMjNSResFun nsr defNS prefix = case XP.lookupNamespaceURI nsr prefix of
      Nothing -> defNS
      Just ns -> ns

type ParseEnv = {
  doc :: Document
, ns :: String
, recNode :: Node
, recElem :: Element
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
    Nothing -> throw "Could not find <record> node!"
    Just nd -> pure nd
  recElem <- case fromNode recNode of
    Nothing -> throw "<record> node could not be cast to an element!"
    Just el -> pure el
  recNS <- getDefaultNS $ Just recElem
  nsRes <- getMetajeloResolver recNode recDoc
  defEvals <- pure $ mkMetajeloXpathEval recDoc (Just nsRes)
  pure $ {
      doc: recDoc
    , ns : recNS
    , recNode: recNode
    , recElem: recElem
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

-- | Used to get a node we know should be there, but still returns
-- | an error message in the event of failure (e.g., for a bad)
-- | XML document.
unsafeSingleNodeValue :: ParseEnv -> Node -> String -> Effect Node
unsafeSingleNodeValue env ctxtNode xpath = do
  nodeMay <- env.xeval.nodeMay ctxtNode xpath
  case nodeMay of
    Just nd -> pure nd
    Nothing -> throw $ nodeErrMsg xpath
  where
    nodeErrMsg nodePath = "Couldn't find required node at: " <> nodePath
