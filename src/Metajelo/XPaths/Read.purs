module Metajelo.XPaths.Read where

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

elemXmlns :: Element -> Effect (Maybe String)
elemXmlns elem = getAttribute "xmlns" elem

nodeXmlns :: Node -> Effect (Maybe String)
nodeXmlns node = case fromNode node of
  Nothing -> pure Nothing
  Just elem -> elemXmlns elem

readRecord :: ParseEnv -> Effect MetajeloRecord
readRecord env = do
  recId <- readIdentifier env
  recDate  <- readDate env
  recModDate <- readModDate env
  recRelIds <- readRelIdentifiers env
  recProds <- readSupplementaryProducts env
  pure $ {
      identifier: recId
    , date: recDate
    , lastModified: recModDate
    , relatedIdentifiers: recRelIds
    , supplementaryProducts: recProds
  }

readIdentifier :: ParseEnv -> Effect Identifier
readIdentifier env = do
  recId <- env.xevalRoot.str idRootP
  idTypeStr <- env.xevalRoot.str idTypeRootAP
  idType <- readIdentifierType $ idTypeStr
  pure {id: recId, idType: idType}

readIdentifierType :: String -> Effect IdentifierType
readIdentifierType "ARK" = pure ARK
readIdentifierType "arXiv" = pure ArXiv
readIdentifierType "bibcode" = pure Bibcode
readIdentifierType "DOI" = pure DOI
readIdentifierType "EAN13" = pure EAN13
readIdentifierType "EISSN" = pure EISSN
readIdentifierType "Handle" = pure Handle
readIdentifierType "IGSN" = pure IGSN
readIdentifierType "ISBN" = pure ISBN
readIdentifierType "ISSN" = pure ISSN
readIdentifierType "ISTC" = pure ISTC
readIdentifierType "LISSN" = pure LISSN
readIdentifierType "LSID" = pure LSID
readIdentifierType "PMID" = pure PMID
readIdentifierType "PURL" = pure PURL
readIdentifierType "UPC" = pure UPC
readIdentifierType "URL" = pure URL
readIdentifierType "URN" = pure URN
readIdentifierType unknown =
  throw $ "Unknown IdentifierType: '" <> unknown <> "'"

readDate :: ParseEnv -> Effect XsdDate
readDate env = env.xevalRoot.str dateRootP

readModDate :: ParseEnv -> Effect XsdDate
readModDate env = env.xevalRoot.str lastModRootP

readRelIdentifiers :: ParseEnv -> Effect (NonEmptyArray RelatedIdentifier)
readRelIdentifiers env = do
  idRes <- env.xevalRoot.any
    relIdRootP RT.ordered_node_snapshot_type
  idNodes <- XP.snapshot idRes
  relIds <- sequence $ map getRelIdentifier idNodes
  case NA.fromArray relIds of
    Just narr -> pure narr
    Nothing -> throw "At least one relatedIdentifier is required!"
  where
    getRelId :: Node -> Effect String
    getRelId nd = env.xeval.str nd "."
    getRelIdType :: Node -> Effect IdentifierType
    getRelIdType nd = do
      idTypeStr <- env.xeval.str nd relIdTypeAP
      readIdentifierType idTypeStr
    getRelRelType :: Node -> Effect RelationType
    getRelRelType nd = do
      idRelStr <- env.xeval.str nd relTypeAP
      readRelationType idRelStr
    getRelIdentifier :: Node -> Effect RelatedIdentifier
    getRelIdentifier nd = do
      recId <- getRelId nd
      idType <- getRelIdType nd
      relType <- getRelRelType nd
      pure {id: recId, idType: idType, relType: relType}

readRelationType :: String -> Effect RelationType
readRelationType "IsCitedBy" = pure IsCitedBy
readRelationType "Cites" = pure Cites
readRelationType "IsSupplementTo" = pure IsSupplementTo
readRelationType "IsSupplementedBy" = pure IsSupplementedBy
readRelationType "IsContinuedBy" = pure IsContinuedBy
readRelationType "Continues" = pure Continues
readRelationType "IsNewVersionOf" = pure IsNewVersionOf
readRelationType "IsPreviousVersionOf" = pure IsPreviousVersionOf
readRelationType "IsPartOf" = pure IsPartOf
readRelationType "HasPart" = pure HasPart
readRelationType "IsReferencedBy" = pure IsReferencedBy
readRelationType "References" = pure References
readRelationType "IsDocumentedBy" = pure IsDocumentedBy
readRelationType "Documents" = pure Documents
readRelationType "IsCompiledBy" = pure IsCompiledBy
readRelationType "Compiles" = pure Compiles
readRelationType "IsVariantFormOf" = pure IsVariantFormOf
readRelationType "IsOriginalFormOf" = pure IsOriginalFormOf
readRelationType "IsIdenticalTo" = pure IsIdenticalTo
readRelationType "HasMetadata" = pure HasMetadata
readRelationType "IsMetadataFor" = pure IsMetadataFor
readRelationType "Reviews" = pure Reviews
readRelationType "IsReviewedBy" = pure IsReviewedBy
readRelationType "IsDerivedFrom" = pure IsDerivedFrom
readRelationType "IsSourceOf" = pure IsSourceOf
readRelationType unknown =
  throw $ "Unknown RelationType: '" <> unknown <> "'"


readSupplementaryProducts :: ParseEnv -> Effect (NonEmptyArray SupplementaryProduct)
readSupplementaryProducts env = do
  prodsRes <- env.xevalRoot.any sProdRootP RT.ordered_node_snapshot_type
  prodNodes <- XP.snapshot prodsRes
  recProdsArr <- sequence $ map getProduct prodNodes
  case NA.fromArray recProdsArr of
    Just narr -> pure narr
    Nothing -> throw "At least one SupplementaryProduct is required!"
  where
    getProduct :: Node -> Effect SupplementaryProduct
    getProduct nd = do
      basicMetadata <- readBasicMetadata env nd
      resId <- readResourceID env nd
      resourceType <- readResourceType env nd
      format <- readFormats env nd
      resourceMetadataSource <- readResourceMetadataSource env nd
      location <- readLocation env nd
      pure {
          basicMetadata: basicMetadata
        , resourceID: resId
        , resourceType: resourceType
        , format: format
        , resourceMetadataSource: resourceMetadataSource
        , location: location
      }

readBasicMetadata :: ParseEnv -> Node -> Effect BasicMetadata
readBasicMetadata env prodNode = do
  basicMetaNode <- unsafeSingleNodeValue env prodNode basicMetaXpath
  title <- getTitle basicMetaNode
  creator <- getCreator basicMetaNode
  pubYear <- getPublicationYear basicMetaNode
  pure {title: title, creator: creator, publicationYear: pubYear}
  where
    basicMetaXpath = xx basicMetaP
    getTitle nd = env.xeval.str nd $ xx titleP
    getCreator nd = env.xeval.str nd $ xx creatorP
    getPublicationYear nd = env.xeval.str nd $ xx pubYearP

readResourceID :: ParseEnv -> Node -> Effect (Maybe ResourceID)
readResourceID env prodNode = do
  resIdNodeMay <- env.xeval.nodeMay prodNode (xx resIdP)
  resIdMay <- pure $ map getResId resIdNodeMay
  resIdTypeMay <- pure $ map getResIdType resIdNodeMay
  combineIdBits resIdMay resIdTypeMay
  where
    getResId :: Node -> Effect String
    getResId nd = env.xeval.str nd "."
    getResIdType :: Node -> Effect IdentifierType
    getResIdType nd = do
      idTypeStr <- env.xeval.str nd "@relatedIdentifierType"
      readIdentifierType idTypeStr
    combineIdBits :: Maybe (Effect String) -> Maybe (Effect IdentifierType)
      -> Effect (Maybe ResourceID)
    combineIdBits idMay idTypeMay = sequence $ do
      idEff <- idMay
      idTypeEff <- idTypeMay
      pure $ lift2 (\i t -> {id: i, idType: t}) idEff idTypeEff

readResourceType :: ParseEnv -> Node -> Effect ResourceType
readResourceType env prodNode = do
  resTypNode <- unsafeSingleNodeValue env prodNode resTypXpath
  descr <- getDescr resTypNode
  resTypGenStr <- getGenType resTypNode
  resTypGen <- readResourceTypeGeneral resTypGenStr
  pure {description: descr, generalType: resTypGen}
  where
    resTypXpath = "x:resourceType"
    getDescr nd = env.xeval.str nd "."
    getGenType nd = env.xeval.str nd "@resourceTypeGeneral"

readResourceTypeGeneral :: String -> Effect ResourceTypeGeneral
readResourceTypeGeneral "Audiovisual" = pure Audiovisual
readResourceTypeGeneral "Dataset" = pure Dataset
readResourceTypeGeneral "Event" = pure Event
readResourceTypeGeneral "Image" = pure Image
readResourceTypeGeneral "InteractiveResource" = pure InteractiveResource
readResourceTypeGeneral "Model" = pure Model
readResourceTypeGeneral "PhysicalObject" = pure PhysicalObject
readResourceTypeGeneral "ResourceCollection" = pure ResourceCollection
readResourceTypeGeneral "Service" = pure Service
readResourceTypeGeneral "Software" = pure Software
readResourceTypeGeneral "Sound" = pure Sound
readResourceTypeGeneral "Text" = pure Text
readResourceTypeGeneral "Workflow" = pure Workflow
readResourceTypeGeneral "Other" = pure Other
readResourceTypeGeneral unknown =
  throw $ "Unknown ResourceTypeGeneral: '" <> unknown <> "'"

readFormats :: ParseEnv -> Node -> Effect (Array Format)
readFormats env prodNode = do
  formatsRes <- env.xeval.any
    prodNode
    "x:Format/x:format" RT.ordered_node_snapshot_type
  formatNodes <- XP.snapshot formatsRes
  sequence $ map getFormat formatNodes
  where
    getFormat:: Node -> Effect Format
    getFormat nd = env.xeval.str nd "."

readResourceMetadataSource :: ParseEnv -> Node -> Effect (Maybe ResourceMetadataSource)
readResourceMetadataSource env prodNode = do
  resMdSourceNodeMay <- env.xeval.nodeMay prodNode $ xx resMetaSourceP
  resMdSourceMay <- pure $ map (getUrl env ".") resMdSourceNodeMay
  resMdSourceTypeMay <- pure $ map getRelType resMdSourceNodeMay
  combineIdBits resMdSourceMay resMdSourceTypeMay
  where
    getRelType :: Node -> Effect RelationType
    getRelType nd = do
      relTypeStr <- env.xeval.str nd relTypeAP
      readRelationType relTypeStr
    combineIdBits :: Maybe (Effect URL) -> Maybe (Effect RelationType)
      -> Effect (Maybe ResourceMetadataSource)
    combineIdBits urlMay relTypeMay = sequence $ do
      urlEff <- urlMay
      relTypeEff <- relTypeMay
      pure $ lift2 (\u t -> {url: u, relationType: t}) urlEff relTypeEff

readInstitutionID :: ParseEnv -> Node -> Effect InstitutionID
readInstitutionID env locNode = do
  instIdNode <- unsafeSingleNodeValue env locNode $ xx instIdP
  instId <- getInstId instIdNode
  instIdType <- getInstIdType instIdNode
  pure {id: instId, idType: instIdType}
  where
    getInstId :: Node -> Effect String
    getInstId nd = env.xeval.str nd "."
    getInstIdType :: Node -> Effect IdentifierType
    getInstIdType nd = do
      idTypeStr <- env.xeval.str nd idTypeAP
      readIdentifierType idTypeStr

readLocation :: ParseEnv -> Node -> Effect Location
readLocation env prodNode = do
  locNode <- unsafeSingleNodeValue env prodNode $ xx locP
  instID <- readInstitutionID env locNode
  instName <- env.xeval.str locNode $ xx instNameP
  instTypeStr <- env.xeval.str locNode $ xx instTypeP
  instType <- readInstitutionType instTypeStr
  superOrgMay <- getSuperOrg locNode
  instContact <- getInstContact locNode
  instSustain <- getInstitutionSustainability locNode
  instPolicies <- readInstitutionPolicies env locNode
  versioningStr <- env.xeval.str locNode $ xx versioningP
  versioning <- readBoolean versioningStr
  pure {
      institutionID: instID
    , institutionName: instName
    , institutionType: instType
    , superOrganizationName: superOrgMay
    , institutionContact: instContact
    , institutionSustainability: instSustain
    , institutionPolicies: instPolicies
    , versioning: versioning
  }
  where
    getSuperOrg :: Node -> Effect (Maybe String)
    getSuperOrg locNode = do
      suorOrgNodeMay <- env.xeval.nodeMay locNode (xx superOrgNameP)
      sequence $ map (\nd -> env.xeval.str nd ".") suorOrgNodeMay
    getInstContact :: Node -> Effect InstitutionContact
    getInstContact locNode = do
      instContactNode <- unsafeSingleNodeValue env locNode instContactNodeName
      contactTypeStr <- env.xeval.str instContactNode instContactTypeAP
      contactType <- readInstitutionContactType contactTypeStr
      contactEmailStr <- env.xeval.str instContactNode "."
      contactEmail <- case validate contactEmailStr of
        Left errMsg -> throw $
          "Error in validating email address for InstitutionContact: "
          <> errMsg
        Right ea -> pure ea
      pure {emailAddress: contactEmail, contactType: contactType}
      where
        instContactNodeName = xx instContactP
    getInstitutionSustainability :: Node -> Effect InstitutionSustainability
    getInstitutionSustainability locNode = do
      iSustainNode <- unsafeSingleNodeValue env locNode $ xx instSustainP
      msURL <- getUrl env (xx missionUrlP) iSustainNode
      fsURL <- getUrl env (xx fundingUrlP) iSustainNode
      pure {
          missionStatementURL : msURL
        , fundingStatementURL : fsURL
      }

readInstitutionType :: String -> Effect InstitutionType
readInstitutionType "commercial" = pure Commercial
readInstitutionType "non-profit" = pure NonProfit
readInstitutionType "governmental" = pure Governmental
readInstitutionType unknown =
  throw $ "Unknown InstitutionType: '" <> unknown <> "'"

readInstitutionContactType :: String -> Effect (Maybe InstitutionContactType)
readInstitutionContactType "dataCustodian" = pure $ Just DataCustodian
readInstitutionContactType "" = pure $ Nothing
readInstitutionContactType unknown =
  throw $ "Unknown InstitutionContactType: '" <> unknown <> "'"

readInstitutionPolicies :: ParseEnv -> Node -> Effect (NonEmptyArray InstitutionPolicy)
readInstitutionPolicies env locNode = do
  polsRes <- env.xeval.any locNode polsNodePath RT.ordered_node_snapshot_type
  polNodes <- XP.snapshot polsRes
  policies <- sequence $ map getInstPolicy polNodes
  case NA.fromArray policies of
    Just narr -> pure narr
    Nothing -> throw "At least one institutionPolicy is required!"
  where
    polsNodePath = xx instPoliciesP /? instPolicyP
    getInstPolicy :: Node -> Effect InstitutionPolicy
    getInstPolicy polNode = do
      policyChildNodeList <- childNodes polNode
      policyChildren <- toArray policyChildNodeList
      policyChildMay <- policyChildren
        # filter (\nd -> not $ startsWith "#" $ nodeName nd)
        # head
        # pure
      policyChild :: Node <- case policyChildMay of
        Just pc -> pure pc
        Nothing -> throw $ "Couldn't find child node of " <> (nodeName polNode)
      policyChildStr <- env.xeval.str policyChild "."
      policy <- case map localName $ fromNode policyChild of
        Just "freeTextPolicy" -> pure $ FreeTextPolicy policyChildStr
        Just "refPolicy" ->  case URL.parsePublicURL policyChildStr of
           Left errMsg -> throw $ "In refPolicy URL parsing: " <> errMsg
           Right url -> pure $ RefPolicy url
        Just other -> throw $ "invalid element '" <> other <>
          "' as child of institutionPolicy"
        Nothing -> throw $ "unable to convert policy child Node with name '"
          <>  nodeName policyChild <> "' to an Element"
      policyTypeStr <- env.xeval.str polNode polTypeAP
      policyType <- readPolicyType policyTypeStr
      appliesToProdStr <- env.xeval.str polNode appliesToProdAP
      appliesToProd <- readBooleanMay appliesToProdStr
      pure {policy: policy, policyType: policyType, appliesToProduct: appliesToProd}

readPolicyType :: String -> Effect (Maybe PolicyType)
readPolicyType "Access" = pure $ Just Access
readPolicyType "Collection" = pure $ Just Collection
readPolicyType "Data" = pure $ Just Data
readPolicyType "Metadata" = pure $ Just Metadata
readPolicyType "Preservation" = pure $ Just Preservation
readPolicyType "Submission" = pure $ Just Submission
readPolicyType "Quality" = pure $ Just Quality
readPolicyType "Terms of Use" = pure $ Just TermsOfUse
readPolicyType "" = pure $ Nothing
readPolicyType unknown =
  throw $ "Unknown PolicyType: '" <> unknown <> "'"

readBoolean :: String -> Effect Boolean
readBoolean "0" = pure false
readBoolean "1" = pure true
readBoolean "false" = pure false
readBoolean "true" = pure true
readBoolean unknown =
  throw $ "Invalid xs:boolean value: '" <> unknown <> "'"

readBooleanMay :: String -> Effect (Maybe Boolean)
readBooleanMay "" = pure Nothing
readBooleanMay other = map Just $ readBoolean other

-- | Used to get a node we should be there, but still returns
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

getUrl :: ParseEnv -> String -> Node -> Effect URL
getUrl env xpath nd = do
  urlStr <- env.xeval.str nd xpath
  case URL.parsePublicURL urlStr of
    Left errMsg -> throw errMsg
    Right url -> pure url
