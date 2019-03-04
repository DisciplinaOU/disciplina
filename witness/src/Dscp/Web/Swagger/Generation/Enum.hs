-- | Helpers to derive descriptions of enum-like types
module Dscp.Web.Swagger.Generation.Enum
    ( enumCaseDocDesc
    , errorDocNoDesc
    , errorCaseDocDesc
    , EnumHasDescription (..)
    , gEnumDocDesc
    , setDocEnumDescription
    ) where

import Control.Lens ((?=))
import Data.Aeson (ToJSON, Value (String), encode, toJSON)
import qualified Data.Swagger as S
import qualified Data.Text as T
import Fmt ((+|), (|+))
import Servant (ServantErr (..))

import Dscp.Util.Constructors
import Dscp.Web.Class

-- | Description of one enum value case.
data EnumDescriptionItem = EnumDescriptionItem
    { ediTag  :: Text
    , ediDesc :: Text
    }

-- | Implementation of 'enumDocDescription' which describes simple enum.
enumCaseDocDesc
    :: forall a.
       (CanEnlistConstructors EnumLikeOnly a, ToJSON a, HasCallStack)
    => Proxy a -> (a -> Text) -> [EnumDescriptionItem]
enumCaseDocDesc _ mkDesc =
    enlistConstructors @EnumLikeOnly @a <&> \x ->
        EnumDescriptionItem
        { ediTag = mkTag x
        , ediDesc = mkDesc x
        }
  where
    mkTag x = case toJSON x of
        -- Most common case for enums.
        -- Unwrapping 'String' to avoid surrounding quotes.
        String v -> v
        -- And here we do a madness.
        other    -> decodeUtf8 $ encode other

-- | Description of error's HTTP code.
showErrorHttpCode :: ToServantErr err => err -> Text
showErrorHttpCode err =
    let code = errHTTPCode $ toServantErrNoBody err
    in show code <> " code"

-- | Whether this error is server-internal and should not appear in the doc.
isInternalError :: ToServantErr err => err -> Bool
isInternalError = (== 500) . errHTTPCode . toServantErrNoBody

-- | Set error description without detailed explanation.
errorDocNoDesc :: ToServantErr err => err -> Maybe EnumDescriptionItem
errorDocNoDesc err =
    guard (not $ isInternalError err) $>
        EnumDescriptionItem
        { ediTag = errorTag err
        , ediDesc = showErrorHttpCode err
        }

-- | Implementation of 'enumDocDescription' which describes single
-- error case.
errorCaseDocDesc
    :: forall fill err.
       (CanEnlistConstructors fill err, ToServantErr err, HasCallStack)
    => Proxy err -> (err -> Text) -> [EnumDescriptionItem]
errorCaseDocDesc _ mkDesc =
    catMaybes $ enlistConstructors @fill @err <&> \err ->
        guard (not $ isInternalError err) $>
            EnumDescriptionItem
            { ediTag = errorTag err
            , ediDesc = mkDesc err <> " (" <> showErrorHttpCode err <> ")"
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
    let descs = enumDocDescription (Proxy @err)
    let desc = T.unlines $ map fmt descs
    S.description ?= desc
    S.enum_ ?= map (toJSON . ediTag) descs
  where
    fmt EnumDescriptionItem{..} =
        "* `" +| ediTag |+ "`: " +| ediDesc |+ ""
