module Test.Main where

import Prelude

import Data.Array                        ((!!), length)
import Data.Array.NonEmpty               as DAN
import Data.Either                       (fromRight)
import Data.Maybe                        (Maybe(..), fromJust, isJust)
-- import Data.Natural                      (intToNat)
-- import Debug.Trace                       (traceM)
import Data.XPath                        (class XPathLike, root, xx, (/?), (//))
import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow)
-- import Foreign                           (isUndefined, isNull, unsafeToForeign)
import Partial.Unsafe                    (unsafePartial)
import Test.Data                         as TD
import Test.Unit                         (suite, test, testSkip)
import Test.Unit.Main                    (runTest)
import Test.Unit.Assert                  as Assert
import Text.Email.Validate               as EA
import URL.Validator                     as URL
import Web.DOM.Document                  (Document, toNode)
import Web.DOM.DOMParser                 (DOMParser, makeDOMParser, parseXMLFromString)
import Web.DOM.XMLSerializer             (XMLSerializer, makeXMLSerializer
                                         , serializeToString)
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

main :: Effect Unit
main = do
  mainTest

mainTest :: Effect Unit
mainTest = runTest do
  suite "XPath Construction" do
    test "institutionPolicies" do
      expected <- pure "x:institutionPolicies/x:institutionPolicy"
      actual <- pure $ xx MX.instPolicyCP /? MX.instPolicyP
      Assert.equal expected actual
  suite "Metajelo.XPaths.Read (with version prefix)" do
    test "Metajelo Parsing" do
      parseEnv <- liftEffect $ MX.getDefaultParseEnv TD.metajeloXmlPrefixed
      record <- liftEffect $ MXR.readRecord parseEnv
      Assert.equal "identifier0" record.identifier.id
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
      Assert.equal "OjlTjf" record.identifier.id
      Assert.equal MJ.EISSN record.identifier.idType
      Assert.equal "2020-04-04" record.date
      Assert.equal "2019-05-04Z" record.lastModified
      Assert.equal 2 (DAN.length record.relatedIdentifiers)
      relId1 <- pure $ unsafePartial fromJust $ record.relatedIdentifiers DAN.!! 1
      Assert.equal "sm3AM1NbOSx" relId1.id
      Assert.equal MJ.PMID  relId1.idType
      Assert.equal MJ.IsNewVersionOf relId1.relType
      prod0 <- pure $ unsafePartial $ fromJust $
        record.supplementaryProducts DAN.!! 0
      prod1 <- pure $ unsafePartial $ fromJust $
        record.supplementaryProducts DAN.!! 1
      prod0resId <- pure $ unsafePartial $ fromJust prod0.resourceID
      Assert.equal MJ.IGSN prod0resId.idType
      Assert.equal "bW8w2m5bzZ0WoKj7SBI_" prod0resId.id
      Assert.equal "niBi6PpDgbhM3" prod0.basicMetadata.title
      Assert.equal "cbK1" prod0.basicMetadata.creator
      Assert.equal "2019-08-11Z" prod0.basicMetadata.publicationYear
      Assert.equal MJ.Event prod0.resourceType.generalType
      Assert.equal "cNMAxYjF0j0k" prod0.resourceType.description
      prod0mdSource <- pure $ unsafePartial $ fromJust prod0.resourceMetadataSource
      Assert.equal "http://HgMuxvbx.au/" (URL.urlToString prod0mdSource.url)
      Assert.equal MJ.HasMetadata prod0mdSource.relationType
      Assert.equal 2 (length prod0.format)
      prod0format1 <- pure $ unsafePartial fromJust $ prod0.format !! 1
      Assert.equal "Vf5ti6" prod0format1
      Assert.equal MJ.ARK prod0.location.institutionID.idType
      Assert.equal "institutionID0" prod0.location.institutionID.id
      Assert.equal "pKhb" prod0.location.institutionName
      Assert.equal MJ.Commercial prod0.location.institutionType
      Assert.equal (Just "DHv5J4LquWfN42iu1a") prod0.location.superOrganizationName
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
      Assert.equal (MJ.FreeTextPolicy "fqxRlcso3") prod0pol1.policy
      Assert.equal (Just MJ.Preservation) prod0pol1.policyType
      Assert.equal (Just true) prod0pol1.appliesToProduct
      Assert.equal true prod0.location.versioning
      Assert.equal true prod1.location.versioning
      -- pure unit
  suite "Metajelo.XPaths.Write" do
    test "Metajelo Writing (individual fields)" do
      env <- liftEffect $ MX.getDefaultParseEnv TD.metajeloXml
      xmlSrlzr <- liftEffect makeXMLSerializer
      -- Testing identifier creation
      idNew <- pure {id: "FooBar", idType: MJ.PURL}
      id0 <- liftEffect $ MXR.readIdentifier env
      liftEffect $ MXW.writeIdentifier env idNew
      id1 <- liftEffect $ MXR.readIdentifier env
      Assert.assert ("id0 == idNew: " <> (show id0) <> (show idNew )) $ id0 /= idNew
      Assert.assert ("id1 /= idNew: " <> (show id1) <> (show idNew )) $ (id1 == idNew)
      -- Testing related idenitfier creation
      newRelId :: MJ.RelatedIdentifier <- pure {
        id : "Dog_Cat_Fox"
      , idType : MJ.EAN13
      , relType : MJ.IsPreviousVersionOf
      }
      liftEffect $ MXW.writeRelIdentifiers env $ DAN.singleton newRelId
      relTestRec <- liftEffect $ MXR.readRecord env
      --curDoc <- liftEffect $ serializeToString env.doc xmlSrlzr -- DEBUG
      --tlog $ "DEBUG:\n" <> curDoc
      Assert.equal 3 (DAN.length relTestRec.relatedIdentifiers)
      relId3 <- pure $ unsafePartial fromJust $ relTestRec.relatedIdentifiers DAN.!! 2
      Assert.equal newRelId.id relId3.id
      Assert.equal newRelId.idType relId3.idType
      Assert.equal newRelId.relType relId3.relType
      Assert.equal "true" (show true) -- for writing location.versioning

      pure unit
    -- TODO: currently testSkip:
    testSkip "Metajelo Writing (entire record, round trip)" do
      writeEnv <- liftEffect $ MX.getDefaultParseEnv MXW.blankDoc
      readEnv <- liftEffect $ MX.getDefaultParseEnv TD.metajeloXmlPrefixed
      rec0 <- liftEffect $ MXR.readRecord readEnv
      liftEffect $ MXW.writeRecord writeEnv rec0
      rec1 <- liftEffect $ MXR.readRecord writeEnv
      Assert.assert ("rec0 /= rec1: " <> (show rec0) <> (show rec1)) $ rec0 == rec1
      pure unit

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

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow

