-- | This module contains additional information about
-- | the Metajelo Schema.
module Metajelo.SchemaInfo where

import Foreign.Object as FO

dateDescr :: String
dateDescr = "The date of the original creation of this metadata record"

formatDescr :: String
formatDescr = "Use file extension or MIME type where possible."

institutionPoliciesDescr :: String
institutionPoliciesDescr = "set of possible policies for this location"

lastModifiedDescr :: String
lastModifiedDescr = "The date of the most recent modification of this recocrd"

locationDescr :: String
locationDescr = "supplementary product was deposited"

recordDescr :: String
recordDescr = "This is the root element. Defined as metadata describing the linkage of a pulication to supplementary products (data, software, etc.)"

resourceTypeDescr :: String
resourceTypeDescr = "same structure as in DataCite"

supplementaryProductDescr :: String
supplementaryProductDescr = "Examples are software, data, audio, video, etc."

supplementaryProductsDescr :: String
supplementaryProductsDescr = "The link to the set of supplemenary products"

descrMap :: FO.Object String
descrMap = FO.fromHomogeneous {
  date: dateDescr
, format: formatDescr
, institutionPolicies: institutionPoliciesDescr
, lastModified: lastModifiedDescr
, location: locationDescr
, record: recordDescr
, resourceType: resourceTypeDescr
, supplementaryProduct: supplementaryProductDescr
, supplementaryProducts: supplementaryProductsDescr
}
