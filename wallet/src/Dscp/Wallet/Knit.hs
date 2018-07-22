module Dscp.Wallet.Knit where

import Prelude hiding (preview)

import Codec.Serialise (deserialiseOrFail)
import Control.Exception
import Control.Lens
import Dscp.Core (addrFromText, txId)
import Dscp.Crypto (mkPassPhrase)
import IiExtras
import Text.Earley

import qualified Data.ByteString.Lazy as BSL
import qualified Serokell.Util.Base64 as Base64
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Dscp.Wallet.Face

import Knit

-- Component type for Knit
data Wallet

data instance ComponentValue _ Wallet
  = ValueAddress Address
  | ValueCoin Coin
  | ValueSecretKey ByteString
  deriving (Eq, Ord, Show)

makePrisms 'ValueAddress

instance Elem components Wallet => ComponentInflate components Wallet where
  componentInflate = \case
    ValueAddress a -> ExprLit $ toLit (LitAddress a)
    ValueCoin c -> ExprLit $ toLit (LitCoin c)
    ValueSecretKey sk -> ExprLit $ toLit (LitSecretKey sk)

data instance ComponentLit Wallet
  = LitAddress Address
  | LitCoin Coin
  | LitSecretKey ByteString
  deriving (Eq, Ord, Show)

data instance ComponentToken Wallet
  = TokenAddress Address
  | TokenCoin Coin
  | TokenSecretKey ByteString
  deriving (Eq, Ord, Show)

makePrisms 'TokenAddress

instance Elem components Wallet => ComponentTokenizer components Wallet where
  componentTokenizer =
      [ toToken . TokenAddress <$> pAddress
      , toToken . TokenCoin <$> pCoin
      , toToken . TokenSecretKey <$> pSecretKey
      ]
    where
      pAddress :: Tokenizer Address
      pAddress = do
        str <- pSomeAlphaNum
        either (fail . show) return $ addrFromText str

      pCoin :: Tokenizer Coin
      pCoin = Coin <$> P.signed (return ()) P.decimal

      pSecretKey :: Tokenizer ByteString
      pSecretKey = do
        str <- (<>) <$> P.takeP (Just "base64") 91 <*> P.string "="
        either (fail . show) return $ Base64.decode str

instance ComponentDetokenizer Wallet where
  componentTokenRender = \case
    TokenAddress a -> pretty a
    TokenCoin c -> pretty c
    TokenSecretKey sk -> Base64.encode sk

instance Elem components Wallet => ComponentLitGrammar components Wallet where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprismElem . _TokenAddress)
      , toLit . LitCoin <$> tok (_Token . uprismElem . _TokenCoin)
      , toLit . LitSecretKey <$> tok (_Token . uprismElem . _TokenSecretKey)
      ]

instance ComponentPrinter Wallet where
  componentPpLit = \case
    LitAddress x -> text (componentTokenRender (TokenAddress x))
    LitCoin x -> text (componentTokenRender (TokenCoin x))
    LitSecretKey x -> text (componentTokenRender (TokenSecretKey x))
  componentPpToken = \case
    TokenAddress _ -> "address"
    TokenCoin _ -> "coin"
    TokenSecretKey _ -> "encrypted secret key"

data instance ComponentCommandRepr components Wallet
  = CommandAction (WalletFace -> IO (Value components))

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitCoin x -> ValueCoin x
    LitSecretKey x -> ValueSecretKey x

data instance ComponentExecContext _ _ Wallet =
  WalletExecCtx WalletFace

instance MonadIO m => ComponentCommandExec m components Wallet where
  componentCommandExec (WalletExecCtx walletFace) (CommandAction act) =
    liftIO $ act walletFace

instance AllConstrained (Elem components) '[Wallet, Core] => ComponentCommandProcs components Wallet where
  componentCommandProcs =
    [
      CommandProc
        { cpName = "gen-key-pair"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            passPhrase <- getArg tyString "passphrase"
            pure (passPhrase)
        , cpRepr = \(passPhrase) -> CommandAction $ \WalletFace{..} -> do
            pp <- either throwIO return . mkPassPhrase . encodeUtf8 $ passPhrase
            (pk, sk) <- walletGenKeyPair pp
            return . toValue . ValueList $
              [ toValue . ValueString . pretty $ pk
              , toValue . ValueString . pretty $ sk
              ]
        , cpHelp = "Generate a key pair."
        }
    , CommandProc
        { cpName = "send-tx"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            passPhrase <- getArg tyString "passphrase"
            secretKey <- getArg tySecretKey "secretkey"
            address <- getArg tyAddress "address"
            value <- getArg tyCoin "value"
            pure (passPhrase, secretKey, address, value)
        , cpRepr = \(passPhrase, secretKey, address, value) -> CommandAction $ \WalletFace{..} -> do
            pp <- either throwIO return . mkPassPhrase . encodeUtf8 $ passPhrase
            sk <- either throwIO return . deserialiseOrFail $ BSL.fromStrict secretKey
            toValue . ValueString . show . txId <$> walletSendTx pp sk address value
        , cpHelp = "Send a transaction."
        }
    , CommandProc
        { cpName = "get-balance"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            address <- getArg tyAddress "address"
            pure (address)
        , cpRepr = \(address) -> CommandAction $ \WalletFace{..} -> do
            toValue . ValueCoin <$> walletGetBalance address
        , cpHelp = "Get address balance."
        }
    ]

----------------------------------------------------------------------------
-- Type projections
----------------------------------------------------------------------------

tyAddress :: Elem components Wallet => TyProjection components Address
tyAddress = TyProjection "Address" (preview _ValueAddress <=< fromValue)

tyCoin :: Elem components Wallet => TyProjection components Coin
tyCoin = TyProjection "Coin" (preview _ValueCoin <=< fromValue)

tySecretKey :: Elem components Wallet => TyProjection components ByteString
tySecretKey = TyProjection "SecretKey" (preview _ValueSecretKey <=< fromValue)
