-- | This module contains additional information about
-- | the Metajelo Schema.
module Metajelo.SchemaInfo where

import Foreign.Object as FO



appliesToProductAttrDscr :: String
appliesToProductAttrDscr = "appliesToProduct is true for policies that apply to this product. It may be absent, for example, if the creator of the record has pulled policies for an an institution from an external service (e.g. re3data), and has not explicitly annotated all the entries as applying to the product (or not)."

locationTypeCTypDscr :: String
locationTypeCTypDscr = "set of possible policies for this location"

policyTypeCTypDscr :: String
policyTypeCTypDscr = "appliesToProduct is true for policies that apply to this product. It may be absent, for example, if the creator of the record has pulled policies for an an institution from an external service (e.g. re3data), and has not explicitly annotated all the entries as applying to the product (or not)."

recordTypeCTypDscr :: String
recordTypeCTypDscr = "The link to the set of supplemenary products"

supplementaryProductTypeCTypDscr :: String
supplementaryProductTypeCTypDscr = "supplementary product was deposited"

dateEleDscr :: String
dateEleDscr = "The date of the original creation of this metadata record"

formatEleDscr :: String
formatEleDscr = "Use file extension or MIME type where possible."

institutionPoliciesEleDscr :: String
institutionPoliciesEleDscr = "set of possible policies for this location"

lastModifiedEleDscr :: String
lastModifiedEleDscr = "The date of the most recent modification of this recocrd"

locationEleDscr :: String
locationEleDscr = "supplementary product was deposited"

recordEleDscr :: String
recordEleDscr = "This is the root element. Defined as metadata describing the linkage of a pulication to supplementary products (data, software, etc.)"

resourceTypeEleDscr :: String
resourceTypeEleDscr = "same structure as in DataCite"

supplementaryProductEleDscr :: String
supplementaryProductEleDscr = "Examples are software, data, audio, video, etc."

supplementaryProductsEleDscr :: String
supplementaryProductsEleDscr = "The link to the set of supplemenary products"

identifierTypeSTypDscr :: String
identifierTypeSTypDscr = "The type of the RelatedIdentifier."

relationTypeSTypDscr :: String
relationTypeSTypDscr = "Description of the relationship of the resource being registered (A) and the related resource (B)."

resourceTypeSTypDscr :: String
resourceTypeSTypDscr = "The general type of a resource."

descrAttrMap :: FO.Object String
descrAttrMap = FO.fromHomogeneous {
  appliesToProductAttr: appliesToProductAttrDscr
}

descrCTypMap :: FO.Object String
descrCTypMap = FO.fromHomogeneous {
  locationTypeCTyp: locationTypeCTypDscr
, policyTypeCTyp: policyTypeCTypDscr
, recordTypeCTyp: recordTypeCTypDscr
, supplementaryProductTypeCTyp: supplementaryProductTypeCTypDscr
}

descrEleMap :: FO.Object String
descrEleMap = FO.fromHomogeneous {
  dateEle: dateEleDscr
, formatEle: formatEleDscr
, institutionPoliciesEle: institutionPoliciesEleDscr
, lastModifiedEle: lastModifiedEleDscr
, locationEle: locationEleDscr
, recordEle: recordEleDscr
, resourceTypeEle: resourceTypeEleDscr
, supplementaryProductEle: supplementaryProductEleDscr
, supplementaryProductsEle: supplementaryProductsEleDscr
}

descrSTypMap :: FO.Object String
descrSTypMap = FO.fromHomogeneous {
  identifierTypeSTyp: identifierTypeSTypDscr
, relationTypeSTyp: relationTypeSTypDscr
, resourceTypeSTyp: resourceTypeSTypDscr
}
