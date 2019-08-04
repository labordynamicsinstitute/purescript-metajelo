module Metajelo.XPaths.Write where

import Prelude (Unit, bind, discard, map, pure, show,
unit, ($), (<>), (<#>))

import Data.Array.NonEmpty               (NonEmptyArray)
import Data.Foldable                     (for_)
import Data.Maybe                        (Maybe(..))
import Data.String.NonEmpty              (NonEmptyString)
import Data.String.NonEmpty              as NES
import Data.Traversable                  (sequence)
import Data.XPath                        (xx)
import Effect                            (Effect)

import Metajelo.Types                    (BasicMetadata, Format, Identifier
                                         , IdentifierType, InstitutionContact
                                         , InstitutionID, InstitutionPolicy
                                         , InstitutionSustainability, Location
                                         , MetajeloRecord, Policy(..)
                                         , RelatedIdentifier, ResourceID
                                         , ResourceMetadataSource, ResourceType
                                         , SupplementaryProduct, XsdDate)
import Metajelo.XPaths                   (ParseEnv, appliesToProdAT, basicMetaP
                                         , creatorP, dateRootP, formatCP, formatP
                                         , freeTextPolicyP, fundingUrlP, idP, idTypeAT
                                         , instContactP, instContactTypeAT, instIdP
                                         , instNameP, instPolicyCP, instPolicyP
                                         , instSustainP, instTypeP, lastModRootP, locP
                                         , missionUrlP, polTypeAT, pubYearP, refPolicyP
                                         , relIdP, relIdTypeAT, relTypeAT, resIdP
                                         , resIdTypeAT, resMetaSourceP, resTypeGenAT
                                         , resTypeP, sProdCP, sProdP, superOrgNameP
                                         , titleP, unsafeSingleNodeValue, versioningP)
import Text.Email.Validate               (toString)
import Text.URL.Validate                 (urlToNEString, urlToString)
import Web.DOM.Document                  (createElementNS)
import Web.DOM.Element                   (Element, fromNode, prefix, setAttribute
                                         , toNode)
import Web.DOM.Node                      (Node, appendChild, setTextContent)

toStr :: NonEmptyString -> String
toStr = NES.toString

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
  setTextContent (toStr id.id) parNode
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
  setTextContent (toStr relId.id) nd
  setAttribute relIdTypeAT (show relId.idType) el
  setAttribute relTypeAT (show relId.relType) el
)

writeSupplementaryProducts :: DocWriterRoot (NonEmptyArray SupplementaryProduct)
writeSupplementaryProducts env prods = for_ prods (\p -> writeProduct env p)

writeProduct :: DocWriterRoot SupplementaryProduct
writeProduct env prod = do
  prodContainer <- unsafeSingleNodeValue env env.recNode (xx sProdCP)
  prodNd <- map toNode $ createAppendRecEle env prodContainer sProdP
  writeBasicMetadata env prodNd prod.basicMetadata
  _ <- sequence $ prod.resourceID <#> (\resId -> writeResourceID env prodNd resId)
  writeResourceType env prodNd prod.resourceType
  writeFormats env prodNd prod.format
  _ <- sequence $ prod.resourceMetadataSource <#> (\resMDS ->
    writeResourceMetadataSource env prodNd resMDS)
  writeLocation env prodNd prod.location

writeBasicMetadata :: DocWriter BasicMetadata
writeBasicMetadata env prodNd bm = do
  bmNd <- map toNode $ createAppendRecEle env prodNd basicMetaP
  titleNd <- map toNode $ createAppendRecEle env bmNd titleP
  setTextContent (toStr bm.title) titleNd
  creatorNd <- map toNode $ createAppendRecEle env bmNd creatorP
  setTextContent (toStr bm.creator) creatorNd
  pubYearNd <- map toNode $ createAppendRecEle env bmNd pubYearP
  setTextContent (toStr bm.publicationYear) pubYearNd

writeResourceType :: DocWriter ResourceType
writeResourceType env prodNd resType = do
  resTypeEl <- createAppendRecEle env prodNd resTypeP
  setTextContent resType.description $ toNode resTypeEl
  setAttribute resTypeGenAT (show resType.generalType) resTypeEl

writeFormats :: DocWriter (Array Format)
writeFormats env prodNd formats = do
  fContNd <- map toNode $ createAppendRecEle env prodNd formatCP
  for_ formats (\f -> writeSimpleNode formatP env fContNd f)

writeResourceMetadataSource :: DocWriter ResourceMetadataSource
writeResourceMetadataSource env prodNd resMdSources = do
  resMDSEl <- createAppendRecEle env prodNd resMetaSourceP
  setTextContent (urlToString resMdSources.url) $ toNode resMDSEl
  setAttribute relTypeAT (show resMdSources.relationType) resMDSEl

writeLocation :: DocWriter Location
writeLocation env prodNd loc = do
  locEl <- createAppendRecEle env prodNd locP
  let locNd = toNode locEl
  writeInstitutionID env locNd loc.institutionID
  writeSimpleNode instNameP env locNd loc.institutionName
  writeSimpleNode' instTypeP env locNd (show loc.institutionType)
  _ <- sequence $ loc.superOrganizationName <#> (\sOrg ->
    writeSimpleNode superOrgNameP env locNd sOrg)
  writeInstitutionContact env locNd loc.institutionContact
  writeInstitutionSustainability env locNd loc.institutionSustainability
  writeInstitutionPolicies env locNd loc.institutionPolicies
  writeSimpleNode' versioningP env locNd (show loc.versioning)

writeInstitutionContact :: DocWriter InstitutionContact
writeInstitutionContact env locNd iContact = do
  iContEl <- createAppendRecEle env locNd instContactP
  _ <- sequence $ iContact.contactType <#> (\cType ->
    setAttribute instContactTypeAT (show cType) iContEl)
  setTextContent (toString iContact.emailAddress) $ toNode iContEl

writeInstitutionSustainability :: DocWriter InstitutionSustainability
writeInstitutionSustainability env locNd iSust = do
  iSustNd <- map toNode $ createAppendRecEle env locNd instSustainP
  writeSimpleNode missionUrlP env iSustNd $ urlToNEString iSust.missionStatementURL
  writeSimpleNode fundingUrlP env iSustNd $ urlToNEString iSust.fundingStatementURL

writeInstitutionPolicies :: DocWriter (NonEmptyArray InstitutionPolicy)
writeInstitutionPolicies env locNd iPolicies = do
  iPolContNd <- map toNode $ createAppendRecEle env locNd instPolicyCP
  for_ iPolicies (\p -> writeInstitutionPolicy env iPolContNd p)

writeInstitutionPolicy :: DocWriter InstitutionPolicy
writeInstitutionPolicy env iPolContNd iPol = do
  iPolEl <- createAppendRecEle env iPolContNd instPolicyP
  let iPolNd = toNode iPolEl
  _ <- sequence $ iPol.policyType <#> (\polType ->
    setAttribute polTypeAT (show polType) iPolEl)
  _ <- sequence $ iPol.appliesToProduct <#> (\apToProd ->
    setAttribute appliesToProdAT (show apToProd) iPolEl)
  case iPol.policy of
    FreeTextPolicy polStr -> writeSimpleNode freeTextPolicyP env iPolNd polStr
    RefPolicy urlStr -> writeSimpleNode refPolicyP env iPolNd $ urlToNEString urlStr

----- Utility functions below -----

-- | For creating a simple node with a string value that
-- | has no other children or attributes.
writeSimpleNode :: String -> DocWriter NonEmptyString
writeSimpleNode tag env parentNd str = writeSimpleNode' tag env parentNd $ toStr str

writeSimpleNode' :: String -> DocWriter String
writeSimpleNode' tag env parentNd str = do
  newNd <- map toNode $ createAppendRecEle env parentNd tag
  setTextContent str newNd

writeNodeMay :: NonEmptyString -> Maybe Node -> Effect Unit
writeNodeMay str ndMay = do
  _ <- sequence $ map (setTextContent $ toStr str) ndMay
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
blankDoc = """<?xml version="1.0" encoding="UTF-8"?>
<record xmlns:re3="http://www.re3data.org/schema/2-2"
 xmlns:datacite="http://datacite.org/schema/kernel-4"
 xmlns="http://ourdomain.cornell.edu/reuse/v.01"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://ourdomain.cornell.edu/reuse/v.01 file:/Users/clagoze/Downloads/metajelo-master/schema/xsd/reproMetadata0.7.xsd">
    <identifier></identifier>
    <date></date>
    <lastModified></lastModified>
    <supplementaryProducts>
    </supplementaryProducts>
</record>
"""
