module Test.Main where

import Prelude

import Data.Array                        ((!!), length)
import Data.Array.NonEmpty               as DAN
import Data.Either                       (fromRight)
import Data.Foldable                     (for_)
import Data.Maybe                        (Maybe(..), fromJust, isJust)
import Data.Natural                      (intToNat)
import Data.String.Common                (null)
import Data.String.NonEmpty              (NonEmptyString, toString, unsafeFromString)
import Data.String.Utils                 (startsWith)
-- import Data.Natural                      (intToNat)
-- import Debug.Trace                       (traceM)
import Data.XPath                        (xx, (/?))
import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow)
-- import Foreign                           (isUndefined, isNull, unsafeToForeign)
import Foreign.Object                    as FO
import Partial.Unsafe                    (unsafePartial)
import Test.Data                         as TD
import Test.Unit                         (Test, TestSuite, suite, test {- , testSkip -})
import Test.Unit.Main                    (runTest)
import Test.Unit.Assert                  as Assert
import Text.Email.Validate               as EA
import Text.URL.Validate                 as URL
import Web.DOM.Document                  (Document, toNode)
import Web.DOM.DOMParser                 (DOMParser, makeDOMParser, parseXMLFromString)
import Web.DOM.XMLSerializer             (makeXMLSerializer)
import Web.DOM.Document.XPath            as XP
import Web.DOM.Document.XPath.ResultType as RT
import Web.DOM.Node                      (Node)

import Metajelo.Types                    as MJ
import Metajelo.XPaths                   as MX
import Metajelo.XPaths.Read              as MXR
import Metajelo.XPaths.Write             as MXW

parseMetajeloDoc :: DOMParser -> Effect Document
parseMetajeloDoc dp = unsafePartial $ map fromRight $
  parseXMLFromString TD.metajeloXml dp

parseMetajeloPfxDoc :: DOMParser -> Effect Document
parseMetajeloPfxDoc dp = unsafePartial $ map fromRight $
  parseXMLFromString TD.metajeloXmlPrefixed dp

parseRecXmlnsFakeXmlDoc :: DOMParser -> Effect Document
parseRecXmlnsFakeXmlDoc dp = unsafePartial $ map fromRight $
  parseXMLFromString TD.recXmlnsFakeXml dp

fromStrUnsafe :: String -> NonEmptyString
fromStrUnsafe s = unsafePartial $ unsafeFromString s

main :: Effect Unit
main = do
  mainTest

mainTest :: Effect Unit
mainTest = runTest do
  suite "Generics" do
    test "get all InstitutionContactTypes" do
      allICTs  <- pure MJ.allInstitutionContactTypes
      Assert.equal [MJ.DataCustodian] allICTs
  suite "XPath Construction" do
    test "institutionPolicies" do
      expected <- pure "x:institutionPolicies/x:institutionPolicy"
      actual <- pure $ xx MX.instPolicyCP /? MX.instPolicyP
      Assert.equal expected actual
  suite "Metajelo.XPaths.Read (with version prefix)" do
    test "Metajelo Parsing" do
      parseEnv <- liftEffect $ MX.getDefaultParseEnv TD.metajeloXmlPrefixed
      record <- liftEffect $ MXR.readRecord parseEnv
      assertNESEq "identifier0" record.identifier.identifier
      -- Assert.equal MJ.EISSN record.identifier.idType
      -- Assert.equal "2020-04-04" record.date
      -- Assert.equal "2019-05-04Z" record.lastModified
  suite "Metajelo.XPaths.Read (no prefix)" do
    test "getMetajeloResolver finds xmlns of record" do
      domParser <- liftEffect $ makeDOMParser

      metajeloDoc <- liftEffect $ parseRecXmlnsFakeXmlDoc domParser
      metajeloMay :: Maybe Node <- liftEffect $ MX.recordOfDoc metajeloDoc
      Assert.assert "found record element" (isJust metajeloMay)
      metajelo :: Node <- pure $ case metajeloMay of
        Nothing -> toNode metajeloDoc
        Just nd -> nd

      mjNSresolver <- liftEffect $ MX.getMetajeloResolver metajelo metajeloDoc

      retrievedNSMay <- pure $ XP.lookupNamespaceURI mjNSresolver "dummy"
      retrievedNS <- pure $ case retrievedNSMay of
        Nothing -> "Failure"
        Just ns -> ns

      Assert.assertFalse
        "defaultMetajeloNS should not equal fakeXmlns or test won't work"
        (MX.defaultMetajeloNS == TD.fakeXmlns)
      Assert.equal TD.fakeXmlns retrievedNS

    test "Metajelo Parsing" do
      parseEnv <- liftEffect $ MX.getDefaultParseEnv TD.metajeloXml
      record <- liftEffect $ MXR.readRecord parseEnv
      assertNESEq "OjlTjf" record.identifier.identifier
      Assert.equal MJ.EISSN record.identifier.identifierType
      dateNES <- liftEffect $ MXW.dateTimeToStr record.date
      Assert.assert ("record.date startsWith input date")
        $ startsWith "2020-04-04" (toString dateNES)
      lastModNES <- liftEffect $ MXW.dateTimeToStr record.lastModified
      tlog "lastModNES is: "
      tlog lastModNES
      Assert.assert ("record.lastModified startsWith input date")
        $ startsWith "2019-05-04" (toString lastModNES)
      Assert.equal 2 (DAN.length record.relatedIdentifiers)
      relId1 <- pure $ unsafePartial fromJust $ record.relatedIdentifiers DAN.!! 1
      assertNESEq "sm3AM1NbOSx" relId1.identifier
      Assert.equal MJ.PMID  relId1.identifierType
      Assert.equal MJ.IsNewVersionOf relId1.relationType
      prod0 <- pure $ unsafePartial $ fromJust $
        record.supplementaryProducts DAN.!! 0
      prod1 <- pure $ unsafePartial $ fromJust $
        record.supplementaryProducts DAN.!! 1
      prod0resId <- pure $ unsafePartial $ fromJust prod0.resourceID
      Assert.equal MJ.IGSN prod0resId.identifierType
      assertNESEq "bW8w2m5bzZ0WoKj7SBI_" prod0resId.identifier
      title0 <- pure $ unsafePartial $ fromJust $
        prod0.basicMetadata.titles DAN.!! 0
      assertNESEq "niBi6PpDgbhM3" title0
      creator0 <- pure $ unsafePartial $ fromJust $
        prod0.basicMetadata.creators DAN.!! 0
      assertNESEq "cbK1" creator0
      Assert.equal (intToNat 2019) prod0.basicMetadata.publicationYear
      Assert.equal MJ.Event prod0.resourceType.generalType
      Assert.equal "cNMAxYjF0j0k" prod0.resourceType.description
      prod0mdSource <- pure $ unsafePartial $ fromJust prod0.resourceMetadataSource
      Assert.equal "http://HgMuxvbx.au/" (URL.urlToString prod0mdSource.url)
      Assert.equal MJ.HasMetadata prod0mdSource.relationType
      Assert.equal 2 (length prod0.format)
      prod0format1 <- pure $ unsafePartial fromJust $ prod0.format !! 1
      assertNESEq "Vf5ti6" prod0format1
      Assert.equal MJ.ARK prod0.location.institutionID.identifierType
      assertNESEq "institutionID0" prod0.location.institutionID.identifier
      assertNESEq "pKhb" prod0.location.institutionName
      Assert.equal MJ.Commercial prod0.location.institutionType
      Assert.equal (Just "DHv5J4LquWfN42iu1a") $
        toString <$> prod0.location.superOrganizationName
      Assert.equal (Just MJ.DataCustodian) prod0.location.institutionContact.contactType
      Assert.equal "foo@baz.edu" $ EA.toString prod0.location.institutionContact.emailAddress
      Assert.equal "http://akbNcujU.fz/"
        (URL.urlToString prod0.location.institutionSustainability.missionStatementURL)
      Assert.equal "http://tdjmeVUQ.lm/"
        (URL.urlToString prod0.location.institutionSustainability.fundingStatementURL)
      prod0pol0 <- pure $ unsafePartial fromJust $
        prod0.location.institutionPolicies DAN.!! 0
      refUrlStr <- pure "http://skGHargw.com/"
      refUrl <- pure $ unsafePartial $ fromRight $ URL.parsePublicURL refUrlStr
      Assert.equal (MJ.RefPolicy refUrl) prod0pol0.policy
      Assert.equal (Just MJ.Quality) prod0pol0.policyType
      Assert.equal (Just false) prod0pol0.appliesToProduct
      prod0pol1 <- pure $ unsafePartial fromJust $
        prod0.location.institutionPolicies DAN.!! 1
      Assert.equal (MJ.FreeTextPolicy $ fromStrUnsafe "fqxRlcso3") prod0pol1.policy
      Assert.equal (Just MJ.Preservation) prod0pol1.policyType
      Assert.equal (Just true) prod0pol1.appliesToProduct
      Assert.equal true prod0.location.versioning
      Assert.equal true prod1.location.versioning
      -- pure unit
  suite "Metajelo.XPaths.Write" do
    test "Metajelo Writing (individual fields)" do
      env <- liftEffect $ MX.getDefaultParseEnv TD.metajeloXml
      -- Testing identifier creation
      idNew <- pure {identifier: fromStrUnsafe "FooBar", identifierType: MJ.PURL}
      id0 <- liftEffect $ MXR.readIdentifier env
      liftEffect $ MXW.writeIdentifier env idNew
      id1 <- liftEffect $ MXR.readIdentifier env
      Assert.assert ("id0 == idNew: " <> (show id0) <> (show idNew )) $ id0 /= idNew
      Assert.assert ("id1 /= idNew: " <> (show id1) <> (show idNew )) $ (id1 == idNew)
      -- Testing related identifier creation
      newRelId :: MJ.RelatedIdentifier <- pure {
        identifier : fromStrUnsafe "Dog_Cat_Fox"
      , identifierType : MJ.EAN13
      , relationType : MJ.IsPreviousVersionOf
      }
      liftEffect $ MXW.writeRelIdentifiers env $ DAN.singleton newRelId
      relTestRec <- liftEffect $ MXR.readRecord env
      Assert.equal 3 (DAN.length relTestRec.relatedIdentifiers)
      relId3 <- pure $ unsafePartial fromJust $ relTestRec.relatedIdentifiers DAN.!! 2
      Assert.equal newRelId.identifier relId3.identifier
      Assert.equal newRelId.identifierType relId3.identifierType
      Assert.equal newRelId.relationType relId3.relationType
      Assert.equal "true" (show true) -- for writing location.versioning
    for_ (FO.keys TD.docStringMap) (\k -> roundTripTest k)

  suite "namespaced tests" do
    test "metajelo.xml" do
      domParser <- liftEffect $ makeDOMParser

      metajeloDoc <- liftEffect $ parseMetajeloDoc domParser
      metajelo <- pure $ toNode metajeloDoc

      mjNSresolver <- liftEffect $ MX.getMetajeloResolver metajelo metajeloDoc

      metajeloIdRes <- liftEffect $ XP.evaluate
        "/foo:record/foo:identifier"
        metajelo
        (Just mjNSresolver)
        RT.string_type
        Nothing
        metajeloDoc
      metajeloId <- liftEffect $ XP.stringValue metajeloIdRes
      tlog $ "got metajelo id" <> metajeloId
      Assert.equal RT.string_type (XP.resultType metajeloIdRes)
      Assert.equal "OjlTjf" metajeloId

roundTripTest :: String -> TestSuite
roundTripTest docName =
  test ("Metajelo Writing (entire record, round trip): " <> docName) do
    docStr <- unsafePartial $ pure $ fromJust $ FO.lookup docName TD.docStringMap
    xmlSrlzr <- liftEffect makeXMLSerializer
    writeEnv <- liftEffect $ MX.getDefaultParseEnv MXW.blankDoc
    readEnv <- liftEffect $ MX.getDefaultParseEnv docStr
    rec0 <- liftEffect $ MXR.readRecord readEnv
    liftEffect $ MXW.writeRecord writeEnv rec0
    -- curDoc <- liftEffect $ serializeToString writeEnv.doc xmlSrlzr -- DEBUG
    -- tlog $ "DEBUG:\n" <> curDoc
    rec1 <- liftEffect $ MXR.readRecord writeEnv
    Assert.assert ("rec0 /= rec1: " <> (show rec0) <> (show rec1)) $ rec0 == rec1
    rec1str <- liftEffect $ MXW.recordToString rec1
    Assert.assert "rec1str is null: " $ not $ null rec1str


tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow

assertNESEq :: String -> NonEmptyString -> Test
assertNESEq exp act = Assert.equal exp $ toString act
