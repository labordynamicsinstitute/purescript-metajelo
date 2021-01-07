module Metajelo.XPaths.Read where

import Metajelo.XPaths

import Control.Apply (lift2)
import Control.Bind ((=<<))
import Data.Array (head, filter)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Bounded (bottom)
import Data.Either (Either(..))
import Data.Either.Extra (catLefts, catRights)
import Data.Int (round)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Natural (intToNat)
import Data.String (stripSuffix)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (startsWith)
import Data.Traversable (sequence, traverse)
import Data.XPath (at, xx, (/?))
import Effect (Effect)
import Effect.Exception (throw)
import Global (readInt)
import Metajelo.Types (BasicMetadata, Format, Identifier, IdentifierType(..), InstitutionContact, InstitutionContactType(..), InstitutionID, InstitutionPolicy, InstitutionSustainability, InstitutionType(..), Location, MetajeloRecord, Policy(..), PolicyType(..), RelatedIdentifier, RelationType(..), ResourceID, ResourceMetadataSource, ResourceType, ResourceTypeGeneral(..), SupplementaryProduct, XsdDate)
import Prelude (bind, join, map, not, pure, (==), (#), ($), (<>), (<#>), (<=))
import Text.Email.Validate (validate)
import Text.URL.Validate (URL, parsePublicURL)
import Web.DOM.Document.XPath as XP
import Web.DOM.Document.XPath.ResultType as RT
import Web.DOM.Element (fromNode, localName)
import Web.DOM.Node (Node, childNodes, nodeName)
import Web.DOM.NodeList (toArray)

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
  recIdStr <- env.xevalRoot.str idRootP
  recId <- rightOrThrow $ readNonEmptyString "Identifier" recIdStr
  idTypeStr <- env.xevalRoot.str $ idTypeRootAP
  idType <- rightOrThrow $ readIdentifierType $ idTypeStr
  pure {identifier: recId, identifierType: idType}

readIdentifierType :: String -> Either String IdentifierType
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
  Left $ "Unknown IdentifierType: '" <> unknown <> "'"

readDate :: ParseEnv -> Effect XsdDate
readDate env = do
  dateStr <- env.xevalRoot.str dateRootP
  dateStrNE <- rightOrThrow $ readNonEmptyString "Date" dateStr
  parseDate dateStrNE

readModDate :: ParseEnv -> Effect XsdDate
readModDate env = do
  dateStr <- env.xevalRoot.str lastModRootP
  dateStrNE <- rightOrThrow $ readNonEmptyString "ModDate" dateStr
  parseDate dateStrNE

parseDate :: NonEmptyString -> Effect XsdDate
parseDate dateStrNE = do
  let dateStr = toString dateStrNE
  -- We have to handle some odd edge cases in the browser parser
  dateStrNoZ <- pure $ case stripSuffix (Pattern "Z") dateStr of
    Just noZ -> if S.length noZ <= 10 then noZ <> expandedZ else dateStr
    Nothing -> dateStr
  jsDate <- JSDate.parse dateStrNoZ
  pure $ fromMaybe bottom $ JSDate.toDateTime jsDate
  where
    expandedZ = "T00:00:00.000Z"

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
    getRelId :: Node -> Effect NonEmptyString
    getRelId nd = do
      relIdStr <- env.xeval.str nd "."
      rightOrThrow $ readNonEmptyString "RelatedIdentifier" relIdStr
    getRelIdType :: Node -> Effect IdentifierType
    getRelIdType nd = do
      idTypeStr <- env.xeval.str nd $ at relIdTypeAT
      rightOrThrow $ readIdentifierType idTypeStr
    getRelRelType :: Node -> Effect RelationType
    getRelRelType nd = do
      idRelStr <- env.xeval.str nd $ at relTypeAT
      rightOrThrow $ readRelationType idRelStr
    getRelIdentifier :: Node -> Effect RelatedIdentifier
    getRelIdentifier nd = do
      recId <- getRelId nd
      idType <- getRelIdType nd
      relType <- getRelRelType nd
      pure {identifier: recId, identifierType: idType, relationType: relType}

readRelationType :: String -> Either String RelationType
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
  Left $ "Unknown RelationType: '" <> unknown <> "'"


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
  titles <- rightOrThrow =<< readNEStringArray env titleP basicMetaNode
  creators <- rightOrThrow =<< readNEStringArray env creatorP basicMetaNode
  pubYear <- getPublicationYear basicMetaNode
  pure {titles: titles, creators: creators, publicationYear: pubYear}
  where
    basicMetaXpath = xx basicMetaP
    -- getTitle nd = env.xeval.str nd $ xx titleP
    getCreator nd = env.xeval.str nd $ xx creatorP
    getPublicationYear nd = do
      pyStr <- env.xeval.str nd $ xx pubYearP
      pyNES <- rightOrThrow $ readNonEmptyString "PubYear" pyStr
      pure $ intToNat $ round $ readInt 10 (toString pyNES)

readResourceID :: ParseEnv -> Node -> Effect (Maybe ResourceID)
readResourceID env prodNode = do
  resIdNodeMay <- env.xeval.nodeMay prodNode (xx resIdP)
  resIdMay <- pure $ map getResId resIdNodeMay
  resIdTypeMay <- pure $ map getResIdType resIdNodeMay
  combineIdBits resIdMay resIdTypeMay
  where
    getResId :: Node -> Effect NonEmptyString
    getResId nd = do
      resIdStr <- env.xeval.str nd "."
      rightOrThrow $ readNonEmptyString "ResourceID" resIdStr
    getResIdType :: Node -> Effect IdentifierType
    getResIdType nd = do
      idTypeStr <- env.xeval.str nd $ at resIdTypeAT
      rightOrThrow $ readIdentifierType idTypeStr
    combineIdBits :: Maybe (Effect NonEmptyString) -> Maybe (Effect IdentifierType)
      -> Effect (Maybe ResourceID)
    combineIdBits idMay idTypeMay = sequence $ do
      idEff <- idMay
      idTypeEff <- idTypeMay
      pure $ lift2 (\i t -> {identifier: i, identifierType: t}) idEff idTypeEff

readResourceType :: ParseEnv -> Node -> Effect ResourceType
readResourceType env prodNode = do
  resTypNode <- unsafeSingleNodeValue env prodNode $ xx resTypeP
  descr <- getDescr resTypNode
  resTypGenStr <- getGenType resTypNode
  resTypGen <- rightOrThrow $ readResourceTypeGeneral resTypGenStr
  pure {description: descr, generalType: resTypGen}
  where
    getDescr nd = env.xeval.str nd "."
    getGenType nd = env.xeval.str nd $ at resTypeGenAT

readResourceTypeGeneral :: String -> Either String ResourceTypeGeneral
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
  Left $ "Unknown ResourceTypeGeneral: '" <> unknown <> "'"

readFormats :: ParseEnv -> Node -> Effect (Array Format)
readFormats env prodNode = do
  formatsRes <- env.xeval.any
    prodNode
    (xx formatCP /? formatP)
    RT.ordered_node_snapshot_type
  formatNodes <- XP.snapshot formatsRes
  traverse getFormat formatNodes
  where
    getFormat:: Node -> Effect Format
    getFormat nd = do
      fmtStr <- env.xeval.str nd "."
      rightOrThrow $ readNonEmptyString "Format" fmtStr

readResourceMetadataSource :: ParseEnv -> Node -> Effect (Maybe ResourceMetadataSource)
readResourceMetadataSource env prodNode = do
  resMdSourceNodeMay <- env.xeval.nodeMay prodNode $ xx resMetaSourceP
  resMdSourceMay <- pure $ map (getUrl env ".") resMdSourceNodeMay
  resMdSourceTypeMay <- pure $ map getRelType resMdSourceNodeMay
  combineIdBits resMdSourceMay resMdSourceTypeMay
  where
    getRelType :: Node -> Effect RelationType
    getRelType nd = do
      relTypeStr <- env.xeval.str nd $ at relTypeAT
      rightOrThrow $ readRelationType relTypeStr
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
  pure {identifier: instId, identifierType: instIdType}
  where
    getInstId :: Node -> Effect NonEmptyString
    getInstId nd = do
      idStr <- env.xeval.str nd "."
      rightOrThrow $ readNonEmptyString "InstitutionID" idStr
    getInstIdType :: Node -> Effect IdentifierType
    getInstIdType nd = do
      idTypeStr <- env.xeval.str nd $ at idTypeAT
      rightOrThrow $ readIdentifierType idTypeStr

readLocation :: ParseEnv -> Node -> Effect Location
readLocation env prodNode = do
  locNode <- unsafeSingleNodeValue env prodNode $ xx locP
  instID <- readInstitutionID env locNode
  instName <- join $ (env.xeval.str locNode $ xx instNameP) <#>
    (\str -> rightOrThrow $ readNonEmptyString "Institution Name" str)
  instTypeStr <- env.xeval.str locNode $ xx instTypeP
  instType <- rightOrThrow $ readInstitutionType instTypeStr
  superOrgMay <- getSuperOrg locNode
  instContact <- getInstContact locNode
  instSustain <- getInstitutionSustainability locNode
  instPolicies <- readInstitutionPolicies env locNode
  versioningStr <- env.xeval.str locNode $ xx versioningP
  versioning <- rightOrThrow $ readBoolean versioningStr
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
    getSuperOrg :: Node -> Effect (Maybe NonEmptyString)
    getSuperOrg locNode = do
      suprOrgNodeMay <- env.xeval.nodeMay locNode (xx superOrgNameP)
      traverse (\nd -> sOrgFromStr $ env.xeval.str nd ".") suprOrgNodeMay
    sOrgFromStr :: Effect String -> Effect NonEmptyString
    sOrgFromStr efStr = do
      str <- efStr
      rightOrThrow $ readNonEmptyString "SuperOrg" str
    getInstContact :: Node -> Effect InstitutionContact
    getInstContact locNode = do
      instContactNode <- unsafeSingleNodeValue env locNode instContactNodeName
      contactTypeStr <- env.xeval.str instContactNode $ at instContactTypeAT
      contactType <- rightOrThrow $ readInstitutionContactType contactTypeStr
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

readInstitutionType :: String -> Either String InstitutionType
readInstitutionType "commercial" = pure Commercial
readInstitutionType "non-profit" = pure NonProfit
readInstitutionType "governmental" = pure Governmental
readInstitutionType unknown =
  Left $ "Unknown InstitutionType: '" <> unknown <> "'"

readInstitutionContactType :: String -> Either String (Maybe InstitutionContactType)
readInstitutionContactType "dataCustodian" = pure $ Just DataCustodian
readInstitutionContactType "" = pure $ Nothing
readInstitutionContactType unknown =
  Left $ "Unknown InstitutionContactType: '" <> unknown <> "'"

readInstitutionPolicies :: ParseEnv -> Node -> Effect (NonEmptyArray InstitutionPolicy)
readInstitutionPolicies env locNode = do
  polsRes <- env.xeval.any locNode polsNodePath RT.ordered_node_snapshot_type
  polNodes <- XP.snapshot polsRes
  policies <- sequence $ map getInstPolicy polNodes
  case NA.fromArray policies of
    Just narr -> pure narr
    Nothing -> throw "At least one institutionPolicy is required!"
  where
    polsNodePath = xx instPolicyCP /? instPolicyP
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
      policyChildNes <- rightOrThrow $ readNonEmptyString "Policy" policyChildStr
      policy <- case map localName $ fromNode policyChild of
        Just p | p == freeTextPolicyP -> pure $ FreeTextPolicy policyChildNes
        Just p | p == refPolicyP -> case parsePublicURL policyChildStr of
           Left errMsg -> throw $ "In refPolicy URL parsing: " <> errMsg
           Right url -> pure $ RefPolicy url
        Just other -> throw $ "invalid element '" <> other <>
          "' as child of institutionPolicy"
        Nothing -> throw $ "unable to convert policy child Node with name '"
          <>  nodeName policyChild <> "' to an Element"
      policyTypeStr <- env.xeval.str polNode $ at polTypeAT
      policyType <- rightOrThrow $ readPolicyType policyTypeStr
      appliesToProdStr <- env.xeval.str polNode $ at appliesToProdAT
      appliesToProd <- rightOrThrow $ readBooleanMay appliesToProdStr
      pure {policy: policy, policyType: policyType, appliesToProduct: appliesToProd}

readPolicyType :: String -> Either String (Maybe PolicyType)
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
  Left $ "Unknown PolicyType: '" <> unknown <> "'"

readBoolean :: String -> Either String Boolean
readBoolean "0" = pure false
readBoolean "1" = pure true
readBoolean "false" = pure false
readBoolean "true" = pure true
readBoolean unknown =
  Left $ "Invalid xs:boolean value: '" <> unknown <> "'"

readBooleanMay :: String -> Either String (Maybe Boolean)
readBooleanMay "" = pure Nothing
readBooleanMay other = map Just $ readBoolean other

getUrl :: ParseEnv -> String -> Node -> Effect URL
getUrl env xpath nd = do
  urlStr <- env.xeval.str nd xpath
  case parsePublicURL urlStr of
    Left errMsg -> throw errMsg
    Right url -> pure url

readNEStringArray :: ParseEnv -> String -> Node 
  -> Effect (Either String (NonEmptyArray NonEmptyString))
readNEStringArray env fieldP node = do
  fieldsRes <- env.xeval.any node (xx fieldP) RT.ordered_node_snapshot_type
  fieldNodes <- XP.snapshot fieldsRes
  fieldStrs <- traverse (\nd -> env.xeval.str nd ".") fieldNodes
  let fieldEis = map (\ts -> readNonEmptyString fieldP ts) fieldStrs
  let fieldErrs = catLefts fieldEis
  let fieldsArr = catRights fieldEis
  pure $ readNonEmptyArray (fieldP <> "s") fieldsArr