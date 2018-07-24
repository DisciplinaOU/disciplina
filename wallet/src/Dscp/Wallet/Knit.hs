module Dscp.Wallet.Knit where

import Prelude hiding (preview)

import Codec.Serialise (deserialiseOrFail)
import Control.Exception
import Control.Lens
import Data.Char (isSpace)
import Data.Scientific (toBoundedInteger)
import Dscp.Core (TxOut(..), addrFromText, coinToInteger, txId)
import Dscp.Crypto (mkPassPhrase)
import IiExtras
import Text.Earley

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
  | ValueSecretKey ByteString
  deriving (Eq, Ord, Show)

makePrisms 'ValueAddress

instance (Elem components Core, Elem components Wallet) => ComponentInflate components Wallet where
  componentInflate = \case
      ValueAddress a -> ExprLit $ toLit (LitAddress a)
      ValueCoin c -> ExprLit $ toLit (LitNumber . fromIntegral . coinToInteger $ c)
      ValueTxOut txOut -> ExprProcCall $ ProcCall "tx-out" $ txOutToArgs txOut
      ValueSecretKey sk -> ExprLit $ toLit (LitSecretKey sk)
    where
      txOutToArgs :: TxOut -> [Arg (Expr CommandId components)]
      txOutToArgs TxOut{..} = map ArgPos $
        [ componentInflate $ ValueAddress txOutAddr
        , componentInflate $ ValueCoin txOutValue
        ]

data instance ComponentLit Wallet
  = LitAddress Address
  | LitSecretKey ByteString
  deriving (Eq, Ord, Show)

data instance ComponentToken Wallet
  = TokenAddress Address
  | TokenSecretKey ByteString
  deriving (Eq, Ord, Show)

makePrisms 'TokenAddress

instance Elem components Wallet => ComponentTokenizer components Wallet where
  componentTokenizer =
      [ -- Order is relevant here: Address should go after SecretKey.
        -- Otherwise, all addresses will be parsed as secret keys
        toToken . TokenSecretKey <$> pSecretKey
      , toToken . TokenAddress <$> pAddress
      ]
    where
      pSecretKey :: Tokenizer ByteString
      pSecretKey = do
        str <- P.takeWhile1P (Just "base64") (not . isSpace)
        either (fail . show) return $ Base64.decode str

      pAddress :: Tokenizer Address
      pAddress = do
        str <- pSomeAlphaNum
        either (fail . show) return $ addrFromText str

instance ComponentDetokenizer Wallet where
  componentTokenRender = \case
    TokenAddress a -> pretty a
    TokenSecretKey sk -> Base64.encode sk

instance Elem components Wallet => ComponentLitGrammar components Wallet where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprismElem . _TokenAddress)
      , toLit . LitSecretKey <$> tok (_Token . uprismElem . _TokenSecretKey)
      ]

instance ComponentPrinter Wallet where
  componentPpLit = \case
    LitAddress x -> text (componentTokenRender (TokenAddress x))
    LitSecretKey x -> text (componentTokenRender (TokenSecretKey x))
  componentPpToken = \case
    TokenAddress _ -> "address"
    TokenSecretKey _ -> "encrypted secret key"

data instance ComponentCommandRepr components Wallet
  = CommandAction (WalletFace -> IO (Value components))
  | CommandReturn (Value components)

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitSecretKey x -> ValueSecretKey x

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
        { cpName = "tx-out"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            txOutAddr <- getArg tyAddress "addr"
            txOutValue <- getArg tyCoin "value"
            return TxOut{..}
        , cpRepr = CommandReturn . toValue . ValueTxOut
        , cpHelp = "Construct a transaction output"
        }
    , CommandProc
        { cpName = "send-tx"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            passPhrase <- getArg tyString "passphrase"
            secretKey <- getArg tySecretKey "secretkey"
            outs <- getArgSome tyTxOut "out"
            pure (passPhrase, secretKey, outs)
        , cpRepr = \(passPhrase, secretKey, outs) -> CommandAction $ \WalletFace{..} -> do
            pp <- either throwIO return . mkPassPhrase . encodeUtf8 $ passPhrase
            sk <- either throwIO return . deserialiseOrFail $ BSL.fromStrict secretKey
            toValue . ValueString . show . txId <$> walletSendTx pp sk outs
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

tySecretKey :: Elem components Wallet => TyProjection components ByteString
tySecretKey = TyProjection "SecretKey" (preview _ValueSecretKey <=< fromValue)
