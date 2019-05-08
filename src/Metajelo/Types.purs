-- | This module is derived from the schema of metajelo,
-- | and more directly from the output of XsdToHaskell
-- | being run on the schema.
module Metajelo.Types where

import Prelude

import Data.Array.NonEmpty                  (NonEmptyArray)
import Data.Maybe                           (Maybe)

import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Eq                  (genericEq)
import Data.Generic.Rep.Show                (genericShow)
import Text.Email.Validate                  (EmailAddress)
import URL.Validator                        (URL)

-- | Stand in for xs:date
type XsdDate = String

type Format = String

-- | metadata about the publication and links to unlimited
-- | number of suppementary products
type MetajeloRecord = {
  identifier :: Identifier
, date :: XsdDate
  -- ^ The date of the original creation of this metadata record
, lastModified :: XsdDate
  -- ^ The date of the most recent modification of this recocrd
, relatedIdentifiers :: NonEmptyArray RelatedIdentifier
, supplementaryProducts :: NonEmptyArray SupplementaryProduct
  -- ^ The link to the set of supplemenary products
}
--derive instance eqRecord :: Eq MetajeloRecord

type Identifier = {
  id :: String
, idType :: IdentifierType
}
type ResourceID = Identifier
type InstitutionID = ResourceID

type RelatedIdentifier = {
  id :: String
, idType :: IdentifierType
, relType :: RelationType
}

-- | The type of the Identifier and RelatedIdentifier.
data IdentifierType
  = ARK      -- Archival Resource Key
  | ArXiv    -- arxiv.org
  | Bibcode
  | DOI
  | EAN13
  | EISSN
  | Handle
  | IGSN
  | ISBN
  | ISSN
  | ISTC
  | LISSN
  | LSID
  | PMID
  | PURL
  | UPC
  | URL
  | URN
--derive instance eqIdentifierType :: Eq IdentifierType
derive instance genericIdentifierType :: Generic IdentifierType _
instance showIdentifierType :: Show IdentifierType where
  show ArXiv = "arXiv"
  show Bibcode = "bibcode"
  show other = genericShow other
instance eqIdentifierType :: Eq IdentifierType where
  eq = genericEq

type SupplementaryProduct = {
  basicMetadata :: BasicMetadata
, resourceID :: Maybe ResourceID
, resourceType :: ResourceType
, format :: Array Format
, resourceMetadataSource :: Maybe ResourceMetadataSource
, location :: Location
}
--derive instance eqSupplementaryProduct :: Eq SupplementaryProduct

type BasicMetadata = {
  title :: String
, creator :: String
, publicationYear :: XsdDate
}


type ResourceType = {
  description :: String
, generalType :: ResourceTypeGeneral
}

-- | The general type of a resource.
data ResourceTypeGeneral =
  Audiovisual
  | Dataset
  | Event
  | Image
  | InteractiveResource
  | Model
  | PhysicalObject
  | ResourceCollection
  | Service
  | Software
  | Sound
  | Text
  | Workflow
  | Other
--derive instance EqResourceType :: Eq ResourceType
derive instance genericResourceTypeGeneral :: Generic ResourceTypeGeneral _
instance showResourceTypeGeneral :: Show ResourceTypeGeneral where
  show = genericShow
instance eqResourceTypeGeneral :: Eq ResourceTypeGeneral where
  eq = genericEq

type ResourceMetadataSource = {
  url :: URL
, relationType :: RelationType
}

-- | Description of the relationship of the resource being
--   registered (A) and the related resource (B).
data RelationType =
  IsCitedBy
  | Cites
  | IsSupplementTo
  | IsSupplementedBy
  | IsContinuedBy
  | Continues
  | IsNewVersionOf
  | IsPreviousVersionOf
  | IsPartOf
  | HasPart
  | IsReferencedBy
  | References
  | IsDocumentedBy
  | Documents
  | IsCompiledBy
  | Compiles
  | IsVariantFormOf
  | IsOriginalFormOf
  | IsIdenticalTo
  | HasMetadata
  | IsMetadataFor
  | Reviews
  | IsReviewedBy
  | IsDerivedFrom
  | IsSourceOf
derive instance genericRelationType :: Generic RelationType _
instance showRelationType :: Show RelationType where
  show = genericShow
instance eqRelationType :: Eq RelationType where
  eq = genericEq

type Location = {
  institutionID :: InstitutionID
, institutionName :: String
, institutionType :: InstitutionType
, superOrganizationName :: Maybe String
, institutionContact :: InstitutionContact
, institutionSustainability :: InstitutionSustainability
, institutionPolicies :: NonEmptyArray InstitutionPolicy
  -- ^ set of possible policies for this location
, versioning :: Boolean
}

data InstitutionType =
    Commercial
  | NonProfit
  | Governmental
derive instance genericInstitutionType :: Generic InstitutionType _
instance showInstitutionType :: Show InstitutionType where
  show Commercial = "commercial"
  show NonProfit = "non-profit"
  show Governmental = "governmental"
instance eqInstitutionType :: Eq InstitutionType where
  eq = genericEq

type InstitutionContact = {
  emailAddress :: EmailAddress
, contactType :: Maybe InstitutionContactType
}

ictShow :: InstitutionContactType -> String
ictShow DataCustodian = "dataCustodian"

data InstitutionContactType = DataCustodian
derive instance eqInstitutionContactType :: Eq InstitutionContactType
instance showInstitutionContactType :: Show InstitutionContactType where
  show = ictShow

type InstitutionSustainability = {
  missionStatementURL :: URL
, fundingStatementURL :: URL
}

data PolicyType
  = Access
  | Collection
  | Data
  | Metadata
  | Preservation
  | Submission
  | Quality
  | TermsOfUse
derive instance genericPolicyType :: Generic PolicyType _
instance showPolicyType :: Show PolicyType where
  show TermsOfUse = "Terms of Use"
  show other = genericShow other
instance eqPolicyType :: Eq PolicyType where
  eq = genericEq

type InstitutionPolicy = {
  policy :: Policy
, policyType :: Maybe PolicyType
, appliesToProduct :: Maybe Boolean
}

data Policy
  = FreeTextPolicy String
  | RefPolicy URL
derive instance genericPolicy :: Generic Policy _
instance showPolicy :: Show Policy where
  show = genericShow
derive instance eqPolicy :: Eq Policy
