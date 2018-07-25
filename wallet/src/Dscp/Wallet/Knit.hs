module Dscp.Wallet.Knit where

import Prelude hiding (preview)

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Exception
import Control.Lens
import Data.Char (isSpace)
import Data.Scientific (toBoundedInteger)
import Dscp.Core (TxOut(..), addrFromText, coinToInteger, txId)
import Dscp.Crypto (FromByteArray(..), decrypt, encrypt, mkPassPhrase)
import IiExtras
import Text.Earley

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BSL
import qualified Serokell.Util.Base64 as Base64
import qualified Text.Megaparsec as P

import Dscp.Wallet.Face

import Knit

-- Component type for Knit
data Wallet

data instance ComponentValue _ Wallet
  = ValueAddress Address
  | ValueCoin Coin
  | ValueTxOut TxOut
  | ValueCryptoKey ByteString
  deriving (Eq, Ord, Show)

makePrisms 'ValueAddress

instance (Elem components Core, Elem components Wallet) => ComponentInflate components Wallet where
  componentInflate = \case
      ValueAddress a -> ExprLit $ toLit (LitAddress a)
      ValueCoin c -> ExprLit $ toLit (LitNumber . fromIntegral . coinToInteger $ c)
      ValueTxOut txOut -> ExprProcCall $ ProcCall "tx-out" $ txOutToArgs txOut
      ValueCryptoKey sk -> ExprLit $ toLit (LitCryptoKey sk)
    where
      txOutToArgs :: TxOut -> [Arg (Expr CommandId components)]
      txOutToArgs TxOut{..} = map ArgPos $
        [ componentInflate $ ValueAddress txOutAddr
        , componentInflate $ ValueCoin txOutValue
        ]

data instance ComponentLit Wallet
  = LitAddress Address
  | LitCryptoKey ByteString
  deriving (Eq, Ord, Show)

data instance ComponentToken Wallet
  = TokenAddress Address
  | TokenCryptoKey ByteString
  deriving (Eq, Ord, Show)

makePrisms 'TokenAddress

instance Elem components Wallet => ComponentTokenizer components Wallet where
  componentTokenizer =
      [ -- Order is relevant here: Address should go after SecretKey.
        -- Otherwise, all addresses will be parsed as secret keys
        toToken . TokenCryptoKey <$> pCryptoKey
      , toToken . TokenAddress <$> pAddress
      ]
    where
      pCryptoKey :: Tokenizer ByteString
      pCryptoKey = do
        str <- P.takeWhile1P (Just "base64") (not . isSpace)
        either (fail . show) return $ Base64.decode str

      pAddress :: Tokenizer Address
      pAddress = do
        str <- pSomeAlphaNum
        either (fail . show) return $ addrFromText str

instance ComponentDetokenizer Wallet where
  componentTokenRender = \case
    TokenAddress a -> pretty a
    TokenCryptoKey sk -> Base64.encode sk

instance Elem components Wallet => ComponentLitGrammar components Wallet where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprismElem . _TokenAddress)
      , toLit . LitCryptoKey <$> tok (_Token . uprismElem . _TokenCryptoKey)
      ]

instance ComponentPrinter Wallet where
  componentPpLit = \case
    LitAddress x -> text (componentTokenRender (TokenAddress x))
    LitCryptoKey x -> text (componentTokenRender (TokenCryptoKey x))
  componentPpToken = \case
    TokenAddress _ -> "address"
    TokenCryptoKey _ -> "cryptographic key"

data instance ComponentCommandRepr components Wallet
  = CommandAction (WalletFace -> IO (Value components))
  | CommandReturn (Value components)

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitCryptoKey x -> ValueCryptoKey x

data instance ComponentExecContext _ _ Wallet =
  WalletExecCtx WalletFace

instance MonadIO m => ComponentCommandExec m components Wallet where
  componentCommandExec (WalletExecCtx walletFace) (CommandAction act) =
    liftIO $ act walletFace
  componentCommandExec _ (CommandReturn val) = return val

instance AllConstrained (Elem components) '[Wallet, Core] => ComponentCommandProcs components Wallet where
  componentCommandProcs =
    [
      CommandProc
        { cpName = "gen-key-pair"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            passString <- getArgOpt tyString "pass"
            pure (passString)
        , cpRepr = \(passString) -> CommandAction $ \WalletFace{..} -> do
            mPassPhrase <- forM passString $ either throwIO return . mkPassPhrase . encodeUtf8
            (secretKey, publicKey) <- walletGenKeyPair
            return . toValue . ValueList $
              toValue . ValueCryptoKey <$>
              [ maybe BA.convert (\pp -> BSL.toStrict . serialise . encrypt pp) mPassPhrase $ secretKey
              , BA.convert $ publicKey
              ]
        , cpHelp = "Generate a key pair."
        }
    , CommandProc
        { cpName = "tx-out"
        , cpArgumentPrepare = map
            $ typeDirectedKwAnn "addr" tyAddress
            . typeDirectedKwAnn "value" tyCoin
        , cpArgumentConsumer = do
            txOutAddr <- getArg tyAddress "addr"
            txOutValue <- getArg tyCoin "value"
            return TxOut{..}
        , cpRepr = CommandReturn . toValue . ValueTxOut
        , cpHelp = "Construct a transaction output"
        }
    , CommandProc
        { cpName = "send-tx"
        , cpArgumentPrepare = map
            $ typeDirectedKwAnn "secretkey" tyCryptoKey
            . typeDirectedKwAnn "out" tyTxOut
        , cpArgumentConsumer = do
            passString <- getArgOpt tyString "pass"
            secretKeyString <- getArg tyCryptoKey "secretkey"
            outs <- getArgSome tyTxOut "out"
            pure (passString, secretKeyString, outs)
        , cpRepr = \(passString, secretKeyString, outs) -> CommandAction $ \WalletFace{..} -> do
            let
              decodeSK = either fail return . fromByteArray
              decodeEncryptedSK passPhrase =
                either throwIO return . deserialiseOrFail . BSL.fromStrict >=>
                either throwIO return . decrypt passPhrase

            mPassPhrase <- forM passString $ either throwIO return . mkPassPhrase . encodeUtf8
            secretKey <- maybe decodeSK decodeEncryptedSK mPassPhrase $ secretKeyString
            toValue . ValueString . show . txId <$> walletSendTx secretKey outs
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

tyCoin :: (Elem components Core, Elem components Wallet) => TyProjection components Coin
tyCoin = TyProjection "Coin" (\v -> fromValueCoin v <|> fromValueNumber v)
  where
    fromValueCoin = preview _ValueCoin <=< fromValue
    fromValueNumber = return . Coin <=< toBoundedInteger <=< preview _ValueNumber <=< fromValue

tyTxOut :: Elem components Wallet => TyProjection components TxOut
tyTxOut = TyProjection "TxOut" (preview _ValueTxOut <=< fromValue)

tyCryptoKey :: Elem components Wallet => TyProjection components ByteString
tyCryptoKey = TyProjection "CryptoKey" (preview _ValueCryptoKey <=< fromValue)
