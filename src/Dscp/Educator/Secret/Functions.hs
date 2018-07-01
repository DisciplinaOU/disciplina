
module Dscp.Educator.Secret.Functions
    ( toEducatorSecretJson
    , fromEducatorSecretJson
    ) where

import Dscp.Crypto (PassPhrase, decrypt, encrypt)
import Dscp.Educator.Secret.Error (EducatorSecretError (..))
import Dscp.Educator.Secret.Types (EducatorSecret (..), EducatorSecretJson (..))
import Dscp.Util (leftToThrow)

toEducatorSecretJson :: PassPhrase -> EducatorSecret -> EducatorSecretJson
toEducatorSecretJson pp EducatorSecret{..} =
    let esjEncSecretKey = encrypt pp esSecretKey
    in EducatorSecretJson{..}

fromEducatorSecretJson :: MonadThrow m => PassPhrase -> EducatorSecretJson -> m EducatorSecret
fromEducatorSecretJson pp EducatorSecretJson{..} = do
    esSecretKey <- decrypt pp esjEncSecretKey
        & leftToThrow SecretWrongPassPhraseError
    return EducatorSecret{..}
