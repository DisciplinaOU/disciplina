-- | Helpers to derive descriptions of enum-like types
module Dscp.Web.Swagger.Generation.Enum
    ( errorDocNoDesc
    , errorCaseDocDesc
    , EnumHasDescription (..)
    , gEnumDocDesc
    , setDocEnumDescription
    ) where

import Control.Lens ((?=))
import Data.Aeson (toJSON)
import qualified Data.Swagger as S
import qualified Data.Text as T
import Fmt ((+|), (|+))
import Servant (ServantErr (..))

import Dscp.Util.Constructors
import Dscp.Web.Class

-- | Description of one enum value case.
data EnumDescriptionItem = EnumDescriptionItem
    { ediTag      :: Text
    , ediHttpCode :: Int
    , ediDesc     :: Maybe Text
    }

-- | Set error description without detailed explanation.
errorDocNoDesc :: ToServantErr err => err -> EnumDescriptionItem
errorDocNoDesc err = EnumDescriptionItem
    { ediTag = errorTag err
    , ediHttpCode = errHTTPCode $ toServantErrNoBody err
    , ediDesc = Nothing
    }

-- | Implementation of 'enumDocDescription' which describes single
-- error case.
errorCaseDocDesc
    :: forall fill err.
       (CanEnlistConstructors fill err, ToServantErr err, HasCallStack)
    => Proxy err -> (err -> Text) -> [EnumDescriptionItem]
errorCaseDocDesc _ mkDesc =
    enlistConstructors @fill @err <&> \err ->
        EnumDescriptionItem
        { ediTag = errorTag err
        , ediHttpCode = errHTTPCode $ toServantErrNoBody err
        , ediDesc = Just (mkDesc err)
        }

class EnumHasDescription err where
    -- | Description of every single error which is part of the given exception datatype.
    enumDocDescription :: Proxy err -> [EnumDescriptionItem]

-- | Implementation of 'enumDocDescription' where for each possible constructor
-- you specify description of the error this constructor carries.
gEnumDocDesc
    :: forall err.
       (CanEnlistConstructors UnsafeFiller err, HasCallStack)
    => (err -> [EnumDescriptionItem]) -> Proxy err -> [EnumDescriptionItem]
gEnumDocDesc mkDesc _ =
    mconcat $ map mkDesc (enlistConstructors @UnsafeFiller @err)

-- | Set description of a type which looks like enum in Swagger.
setDocEnumDescription
    :: forall err.
       (EnumHasDescription err)
    => State S.Schema ()
setDocEnumDescription = do
    let descs = dropInternalErrors $ enumDocDescription (Proxy @err)
    let desc = T.unlines $ map fmt descs
    S.description ?= desc
    S.enum_ ?= map (toJSON . ediTag) descs
  where
    dropInternalErrors = filter (\d -> ediHttpCode d /= 500)
    fmt EnumDescriptionItem{..} = mconcat
        [ "* `" +| ediTag |+ "`"
        , case ediDesc of
            Nothing -> ""
            Just d  -> ": " +| d |+ ""
        , " (" +| ediHttpCode |+ " code)"
        ]
