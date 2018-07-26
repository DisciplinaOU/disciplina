module Dscp.Wallet.Knit where

import Prelude hiding (preview)

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Exception
import Control.Lens
import Data.Scientific (toBoundedInteger)
import Dscp.Core (TxOut (..), addrFromText, coinToInteger, mkAddr, toTxId)
import Dscp.Crypto (FromByteArray (..), decrypt, encrypt, mkPassPhrase)
import Dscp.Util (toHex)
import IiExtras
import Text.Earley

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BSL
import qualified Serokell.Util.Base64 as Base64

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
      ValueCryptoKey sk -> ExprLit $ toLit (LitString . Base64.encode $ sk)
    where
      txOutToArgs :: TxOut -> [Arg (Expr CommandId components)]
      txOutToArgs TxOut{..} = map ArgPos $
        [ componentInflate $ ValueAddress txOutAddr
        , componentInflate $ ValueCoin txOutValue
        ]

data instance ComponentLit Wallet
  = LitAddress Address
  deriving (Eq, Ord, Show)

data instance ComponentToken Wallet
  = TokenAddress Address
  deriving (Eq, Ord, Show)

makePrisms 'TokenAddress

instance Elem components Wallet => ComponentTokenizer components Wallet where
  componentTokenizer =
      [ toToken . TokenAddress <$> pAddress
      ]
    where
      pAddress :: Tokenizer Address
      pAddress = do
        str <- pSomeAlphaNum
        either (fail . show) return $ addrFromText str

instance ComponentDetokenizer Wallet where
  componentTokenRender = \case
    TokenAddress a -> pretty a

instance Elem components Wallet => ComponentLitGrammar components Wallet where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprismElem . _TokenAddress)
      ]

instance ComponentPrinter Wallet where
  componentPpLit = \case
    LitAddress x -> text (componentTokenRender (TokenAddress x))
  componentPpToken = \case
    TokenAddress _ -> "address"

data instance ComponentCommandRepr components Wallet
  = CommandAction (WalletFace -> IO (Value components))
  | CommandReturn (Value components)

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x

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
              toValue <$>
              [ ValueCryptoKey . maybe BA.convert (\pp -> BSL.toStrict . serialise . encrypt pp) mPassPhrase $ secretKey
              , ValueCryptoKey . BA.convert $ publicKey
              , ValueAddress . mkAddr $ publicKey
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
            toValue . ValueString . toHex . toTxId <$> walletSendTx secretKey outs
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

tyCryptoKey :: (Elem components Core, Elem components Wallet) => TyProjection components ByteString
tyCryptoKey = TyProjection "Base64" (\v -> fromValueCryptoKey v <|> fromValueString v)
  where
    fromValueCryptoKey = preview _ValueCryptoKey <=< fromValue
    fromValueString = (rightToMaybe . Base64.decode) <=< preview _ValueString <=< fromValue
