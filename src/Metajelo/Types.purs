-- | This module is derived from the schema of metajelo,
-- | and more directly from the output of XsdToHaskell
-- | being run on the schema.
module Metajelo.Types (
  module Exports
, module Metajelo.Types
) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.DateTime (DateTime)
import Data.Enum (class BoundedEnum, class Enum, class SmallBounded, upFromIncluding)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded as GBounded
import Data.Generic.Rep.Enum as GEnum
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Natural (Natural)
import Data.String.NonEmpty (NonEmptyString)
import Data.Unfoldable1 (class Unfoldable1)
import DataCite.Types (Attributes, AttributesRows, Container, ContainerRows, Creator, CreatorRows, Data, DataRows, Relationships, RelationshipsRows, Resource, ResourceRows, SchemaVersion, Title, TitleRows, dataCiteVersion) as Exports
import DataCite.Types.Common (Identifier, RelatedIdentifier, RelationType, ResourceTypeGeneral)
import DataCite.Types.Common (class EnumReadForeign, AltId, BaseIdRows, Identifier, IdentifierType(..), RelatedIdentifier, RelatedIdentifierRows, RelationType(..), ResourceTypeGeneral(..), allIdentifierTypes, allRelationTypes, allResourceTypeGenerals, altIdToId, enumReadForeign, enumReadForeignImpl) as Exports
import Text.Email.Validate (EmailAddress)
import Text.URL.Validate (URL)

-- | Stand in for xs:date
type XsdDate = DateTime

type Format = NonEmptyString

-- | metadata about the publication and links to unlimited
-- | number of suppementary products
type MetajeloRecordRows = (
  identifier :: Identifier
, date :: XsdDate
  -- ^ The date of the original creation of this metadata record
, lastModified :: XsdDate
  -- ^ The date of the most recent modification of this recocrd
, relatedIdentifiers :: NonEmptyArray RelatedIdentifier
, supplementaryProducts :: NonEmptyArray SupplementaryProduct
  -- ^ The link to the set of supplemenary products
)
type MetajeloRecord = Record MetajeloRecordRows

type ResourceID = Identifier
type InstitutionID = Identifier

type SupplementaryProductRows = (
  basicMetadata :: BasicMetadata
, resourceID :: Maybe ResourceID
, resourceType :: ResourceType
, format :: Array Format
, resourceMetadataSource :: Maybe ResourceMetadataSource
, location :: Location
)
type SupplementaryProduct = Record SupplementaryProductRows

--derive instance eqSupplementaryProduct :: Eq SupplementaryProduct

type BasicMetadataRows = (
  titles :: NonEmptyArray NonEmptyString
, creators :: NonEmptyArray NonEmptyString
, publicationYear :: Natural
)
type BasicMetadata = Record BasicMetadataRows

type ResourceTypeRows = (
  description :: String
, generalType :: ResourceTypeGeneral
)
type ResourceType = Record ResourceTypeRows


type ResourceMetadataSourceRows = (
  url :: URL
, relationType :: RelationType
)
type ResourceMetadataSource = Record ResourceMetadataSourceRows

type LocationRows = (
  institutionID :: InstitutionID
, institutionName :: NonEmptyString
, institutionType :: InstitutionType
, superOrganizationName :: Maybe NonEmptyString
, institutionContact :: InstitutionContact
, institutionSustainability :: InstitutionSustainability
, institutionPolicies :: NonEmptyArray InstitutionPolicy
  -- | ^ set of possible policies for this location
, versioning :: Boolean
)
type Location = Record LocationRows

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
instance smallBoundedInstitutionType :: SmallBounded InstitutionType

allInstitutionTypes :: forall u. Unfoldable1 u => u InstitutionType
allInstitutionTypes = upFromIncluding bottom

type InstitutionContactRows = (
  emailAddress :: EmailAddress
, contactType :: Maybe InstitutionContactType
)
type InstitutionContact = Record InstitutionContactRows

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
instance smallBoundedInstitutionContactType :: SmallBounded InstitutionContactType

allInstitutionContactTypes :: forall u. Unfoldable1 u => u InstitutionContactType
allInstitutionContactTypes = upFromIncluding bottom

type InstitutionSustainabilityRows = (
  missionStatementURL :: URL
, fundingStatementURL :: URL
)
type InstitutionSustainability = Record InstitutionSustainabilityRows

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
instance smallBoundedPolicyType :: SmallBounded PolicyType

allPolicyTypes :: forall u. Unfoldable1 u => u PolicyType
allPolicyTypes = upFromIncluding bottom

type InstitutionPolicyRows = (
  policy :: Policy
, policyType :: Maybe PolicyType
, appliesToProduct :: Maybe Boolean
)
type InstitutionPolicy = Record InstitutionPolicyRows

data Policy
  = FreeTextPolicy NonEmptyString
  | RefPolicy URL
derive instance genericPolicy :: Generic Policy _
instance showPolicy :: Show Policy where
  show = genericShow
derive instance eqPolicy :: Eq Policy
