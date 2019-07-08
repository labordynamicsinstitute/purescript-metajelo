-- | This module is derived from the schema of metajelo,
-- | and more directly from the output of XsdToHaskell
-- | being run on the schema.
module Metajelo.Types where

import Prelude

import Data.Array.NonEmpty                  (NonEmptyArray)
import Data.Enum (class BoundedEnum, class Enum, upFromIncluding)
import Data.Maybe                           (Maybe)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Bounded             as GBounded
import Data.Generic.Rep.Eq                  (genericEq)
import Data.Generic.Rep.Enum                as GEnum
import Data.Generic.Rep.Ord                 as GOrd
import Data.Generic.Rep.Show                (genericShow)
import Data.Unfoldable1                     (class Unfoldable1)
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
-- derive instance eqRecord :: Eq MetajeloRecord

type BaseId otherField = {
  id :: String
, idType :: IdentifierType
| otherField
}

type Identifier = BaseId()

-- derive instance eqIdentifier :: Eq Identifier

type ResourceID = Identifier
type InstitutionID = ResourceID

type RelatedIdentifier = BaseId (relType :: RelationType)

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
derive instance genericIdentifierType :: Generic IdentifierType _
instance showIdentifierType :: Show IdentifierType where
  show ArXiv = "arXiv"
  show Bibcode = "bibcode"
  show other = genericShow other
instance eqIdentifierType :: Eq IdentifierType where
  eq = genericEq
instance ordIdentifierType :: Ord IdentifierType where
  compare x y = GOrd.genericCompare x y
instance boundedIdentifierType :: Bounded IdentifierType where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumIdentifierType :: Enum IdentifierType where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumIdentifierType :: BoundedEnum IdentifierType where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

allIdentifierTypes :: forall u. Unfoldable1 u => u IdentifierType
allIdentifierTypes = upFromIncluding bottom

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
instance ordResourceTypeGeneral :: Ord ResourceTypeGeneral where
  compare x y = GOrd.genericCompare x y
instance boundedResourceTypeGeneral :: Bounded ResourceTypeGeneral where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumResourceTypeGeneral :: Enum ResourceTypeGeneral where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumResourceTypeGeneral :: BoundedEnum ResourceTypeGeneral where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

allResourceTypeGenerals :: forall u. Unfoldable1 u => u ResourceTypeGeneral
allResourceTypeGenerals = upFromIncluding bottom


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
instance ordRelationType :: Ord RelationType where
  compare x y = GOrd.genericCompare x y
instance boundedRelationType :: Bounded RelationType where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumRelationType :: Enum RelationType where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumRelationType :: BoundedEnum RelationType where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

allRelationTypes :: forall u. Unfoldable1 u => u RelationType
allRelationTypes = upFromIncluding bottom

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
instance ordInstitutionType :: Ord InstitutionType where
  compare x y = GOrd.genericCompare x y
instance boundedInstitutionType :: Bounded InstitutionType where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumInstitutionType :: Enum InstitutionType where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumInstitutionType :: BoundedEnum InstitutionType where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

allInstitutionTypes :: forall u. Unfoldable1 u => u InstitutionType
allInstitutionTypes = upFromIncluding bottom


type InstitutionContact = {
  emailAddress :: EmailAddress
, contactType :: Maybe InstitutionContactType
}

ictShow :: InstitutionContactType -> String
ictShow DataCustodian = "dataCustodian"

data InstitutionContactType = DataCustodian
derive instance genericInstitutionContactType :: Generic InstitutionContactType _
instance eqInstitutionContactType :: Eq InstitutionContactType where
  eq = genericEq
instance showInstitutionContactType :: Show InstitutionContactType where
  show = ictShow
instance ordInstitutionContactType :: Ord InstitutionContactType where
  compare x y = GOrd.genericCompare x y
instance boundedInstitutionContactType :: Bounded InstitutionContactType where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumInstitutionContactType :: Enum InstitutionContactType where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumInstitutionContactType :: BoundedEnum InstitutionContactType where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

allInstitutionContactTypes :: forall u. Unfoldable1 u => u InstitutionContactType
allInstitutionContactTypes = upFromIncluding bottom

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
instance ordPolicyType :: Ord PolicyType where
  compare x y = GOrd.genericCompare x y
instance boundedPolicyType :: Bounded PolicyType where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumPolicyType :: Enum PolicyType where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumPolicyType :: BoundedEnum PolicyType where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

allPolicyTypes :: forall u. Unfoldable1 u => u PolicyType
allPolicyTypes = upFromIncluding bottom

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
