module Dscp.Snowdrop.Expanders
    ( expandBlock
    , expandGTxs
    , expandGTx
    ) where

import Control.Lens (contramap)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Fmt ((+|), (|+))
import Serokell.Util (enumerate)

import qualified Snowdrop.Block as SD
import Snowdrop.Core (ChgAccum, ChgAccumCtx, ERoComp, Expander (..), SeqExpanders (..),
                      StateTxType (..), ValueOp (..), mkDiffCS, queryOne, queryOneExists)
import Snowdrop.Execution (RestrictCtx, expandUnionRawTxs)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto as Dscp
import Dscp.Snowdrop.AccountValidation
import Dscp.Snowdrop.Configuration
import qualified Dscp.Snowdrop.Configuration as Conf (Proofs (..))
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types

----------------------------------------------------------------------------
-- Top-level expanders
----------------------------------------------------------------------------

-- | Expand block.
expandBlock ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => Bool
    -> Block
    -> ERoComp Exceptions Ids Values ctx SBlock
expandBlock applyFees block@Block{..} = do
    let issuer = hIssuer bHeader
    stateTxs <- expandGTxs applyFees issuer (bbTxs bBody)
    metaStateTx <- expandBlockMetaTx (BlockMetaTx block)
    pure $ SD.Block bHeader (SPayload (metaStateTx : stateTxs) (hash bBody))

expandBlockMetaTx
    :: ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => BlockMetaTx -> ERoComp Exceptions Ids Values ctx SStateTx
expandBlockMetaTx meta =
    let stateTx = StateTxType $ getId (Proxy @TxIds) BlockMetaTxTypeId
        proof = BlockMetaTxWitnessProof
    in expandUnionRawTxs (\_ -> (stateTx, proof, seqExpandersBlockMetaTx)) [meta] >>= \case
           [expanded] -> return expanded
           _          -> error "expandBlockMetaTx: did not expand 1 meta into 1 sd tx"

-- | Expand list of global txs.
expandGTxs ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => Bool
    -> Dscp.PublicKey
    -> [GTxWitnessed]
    -> ERoComp Exceptions Ids Values ctx [SStateTx]
expandGTxs applyFees = expandUnionRawTxs . getByGTx applyFees

-- | Expand one global tx.
expandGTx ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => Dscp.PublicKey
    -> GTxWitnessed
    -> ERoComp Exceptions Ids Values ctx SStateTx
expandGTx pk tx =
    expandUnionRawTxs (getByGTx True pk) [tx] >>= \case
        [expanded] -> return expanded
        _          -> error "expandGTx: did not expand 1 raw tx into 1 sd tx"

getByGTx
    :: HasCoreConfig
    => Bool
    -> Dscp.PublicKey
    -> GTxWitnessed
    -> ( StateTxType
       , Proofs
       , SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed
       )
getByGTx applyFees pk t =
    let fee  = calcFeeG (if applyFees then feeConfig else noFeesConfig) t
        addr = mkAddr pk
        (a, b) = case t of
                   GMoneyTxWitnessed tx        -> toProofBalanceTx fee tx
                   GPublicationTxWitnessed ptx -> toProofPublicationTx fee ptx
    in (a, b, seqExpandersGTx addr fee t)

seqExpandersGTx
    :: HasCoreConfig
    => Address
    -> Fees
    -> GTxWitnessed
    -> SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed
seqExpandersGTx addr minFee (GMoneyTxWitnessed _) =
    flip contramap (seqExpandersBalanceTx addr minFee) $ \(GMoneyTxWitnessed tx) -> tx
seqExpandersGTx addr minFee (GPublicationTxWitnessed _) =
    flip contramap (seqExpandersPublicationTx addr minFee) $ \(GPublicationTxWitnessed ptx) -> ptx

----------------------------------------------------------------------------
-- Publication tx
----------------------------------------------------------------------------

toProofPublicationTx :: Fees -> PublicationTxWitnessed -> (StateTxType, Proofs)
toProofPublicationTx minFee (PublicationTxWitnessed tx (PublicationTxWitness {..})) =
    (txType, Conf.PublicationTxWitnessProof $ WithSignature
        { wsSignature = toDscpSig pwSig
        , wsPublicKey = toDscpPK pwPk
        , wsBody = (toPtxId tx, pwPk, ptHeader)
        } `PersonalisedProof`
            minFee
    )
  where
    PublicationTx { ptHeader } = tx
    txType = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId


seqExpandersPublicationTx
    :: Address
    -> Fees
    -> SeqExpanders Exceptions Ids Proofs Values ctx PublicationTxWitnessed
seqExpandersPublicationTx feesReceiverAddr (Fees minFee) =
    SeqExpanders $ one $ Expander
        (Set.fromList [publicationOfPrefix])
        (Set.fromList [accountPrefix, publicationHeadPrefix, publicationOfPrefix,
                       publicationIdsPrefix, privateBlockTxPrefix])
        $ \ptw@PublicationTxWitnessed { ptwTx } -> do
            let PublicationTx{ ptHeader, ptAuthor, ptFeesAmount } = ptwTx
            let ptxId = hash ptwTx
            let phHash = hash ptHeader
            let prevHash = ptHeader ^. pbhPrevBlock
            let (prevHashM :: Maybe PrivateHeaderHash) =
                    prevHash <$ guard (prevHash /= genesisHeaderHash ptAuthor)

            headerWasEarlier <- queryOneExists (PublicationHead phHash)
            let headerIsLast = prevHash == phHash
            when (headerWasEarlier || headerIsLast) $
                throwLocalError PublicationLocalLoop

            let feeAmount = fromIntegral $ coinToInteger ptFeesAmount
            maybePub <- queryOne (PublicationsOf ptAuthor)
            mFeesReceiver <- queryOne (AccountId feesReceiverAddr)

            let isPaid = ptAuthor /= feesReceiverAddr

            account <- fromMaybe def <$> queryOne (AccountId ptAuthor)

            when (isPaid && feeAmount < coinToInteger minFee) $
                throwLocalError PublicationFeeIsTooLow
                    { peMinimalFee = coinToInteger minFee, peGivenFee = feeAmount }

            when (aBalance account < feeAmount) $
                throwLocalError PublicationCantAffordFee
                    { peFee = feeAmount, peBalance = aBalance account }

            let dIssuer =
                    maybe (New Account { aBalance = feeAmount, aNonce = 0 })
                          (\issuer -> Upd issuer { aBalance = aBalance issuer + feeAmount })
                          mFeesReceiver

            let feesChanges
                    | feeAmount == 0 = []
                    | isPaid =
                          [ AccountId ptAuthor ==>
                                Upd account
                                    -- You can say that `PubOf ==> LastPub`
                                    -- mechanism will prevent double-pub
                                    -- but lets not depend on that impl detail.
                                    { aNonce   = aNonce account + 1
                                    , aBalance = aBalance account - feeAmount
                                    }
                          , AccountId feesReceiverAddr ==> dIssuer ]
                    -- If publication author and fees receiver are the same
                    -- address, we do not transfer any coins.
                    | otherwise = []

            let change = if isJust maybePub then Upd else New
            pure $ mkDiffCS $ Map.fromList $
                [ PublicationsOf  ptAuthor ==> change (LastPublication phHash)
                , PublicationHead phHash   ==> New    (PublicationNext prevHashM)
                , PublicationIds  ptxId    ==> New    (PublicationItself ptw)
                , PrivateBlockTx  phHash   ==> New    (PrivateBlockTxVal ptxId)
                ] ++ feesChanges

----------------------------------------------------------------------------
-- Money tx
----------------------------------------------------------------------------

type Expansion ctx = ERoComp Exceptions Ids Values ctx
type ChangeEntry = (Ids, ValueOp Values)

toProofBalanceTx :: Fees -> TxWitnessed -> (StateTxType, Proofs)
toProofBalanceTx minFee (TxWitnessed tx (TxWitness {..})) =
    (txType, AddressTxWitnessProof $ WithSignature {..} `PersonalisedProof` minFee)
  where
    txType = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId
    wsSignature = toDscpSig txwSig
    wsPublicKey = toDscpPK txwPk
    wsBody = (toTxId tx, txwPk, ())

toChangeMap :: [ChangeEntry] -> Map Ids (ValueOp Values)
toChangeMap changes =
    let keyz = map fst changes
    in if length keyz == length (ordNub keyz)
       then Map.fromList changes
       else error "Changeset produced by tx has duplicates"

seqExpandersBalanceTx :: Address
                      -> Fees
                      -> SeqExpanders Exceptions Ids Proofs Values ctx TxWitnessed
seqExpandersBalanceTx feesReceiverAddr (Fees minimalFees) =
    SeqExpanders $ one $ Expander inP outP $ \tw@TxWitnessed{..} -> do
        let txId = toTxId twTx
        let gTxId = coerce @_ @GTxId txId

        let outputs = txOuts twTx

        -- Account we transfer money from
        let txIn      = txInAcc twTx
        let inAddr    = tiaAddr txIn
        let outSame   = find ((== inAddr) . txOutAddr) (txOuts twTx)
        let outOther  = maybe outputs (`List.delete` outputs) outSame
        let inputBack = maybe 0 (coinToInteger . txOutValue) outSame
        -- How much money user has spent as the tx input.
        let inputSent = coinToInteger $ txInValue twTx

        inputChg <- expandDeparture txIn (inputSent - inputBack)
        -- TODO: shouldn't we pass "otherOutputs" here?
        outputChg <- expandReceivals outputs outOther
        historyChg <- expandTxHistory gTxId (tiaAddr txIn : map txOutAddr outputs)
        remChg <- rememberTx txId tw

        -- Money flow related changes (w/o fees)
        let changes = toChangeMap (inputChg : remChg : outputChg ++ historyChg)

        changesWithFees <- expandAddFees inputSent outputs feesReceiverAddr minimalFees changes
        pure $ mkDiffCS changesWithFees
  where
    -- Account prefixes are used during the computation to access current balance
    inP  = Set.fromList [accountPrefix, txOfPrefix]
    -- Expander returns account changes only
    outP = Set.fromList [accountPrefix, txPrefix, txHeadPrefix, txOfPrefix]

expandDeparture :: TxInAcc -> Integer -> Expansion ctx ChangeEntry
expandDeparture inpAcc inputSentToOthers = do
    -- How much money user has sent to himself back.
    let inAddr = tiaAddr inpAcc

    inpPrevAcc <- fromMaybe def <$> queryOne (AccountId inAddr)

    let inpNewBal     = aBalance inpPrevAcc - inputSentToOthers
    let newInpAccount = Account
            { aBalance = inpNewBal
            , aNonce   = tiaNonce inpAcc + 1
            }

    return (AccountId inAddr ==> Upd newInpAccount)

expandReceivals
    :: [TxOut]
    -> [TxOut]
    -> Expansion ctx [ChangeEntry]
expandReceivals outputs otherOutputs = do
    when (null outputs) $
        throwLocalError MTxNoOutputs

    let uniqOutAddrs = ordNub $ map txOutAddr outputs
    when (length uniqOutAddrs /= length outputs) $
        throwLocalError MTxDuplicateOutputs

    forM otherOutputs $ \TxOut{..} -> do
        prevAcc <- queryOne (AccountId txOutAddr)

        let outVal    = coinToInteger txOutValue
        let newAcc    = Account { aBalance = outVal,               aNonce = 0 }
        let updAcc a0 = Account { aBalance = aBalance a0 + outVal, aNonce = aNonce a0 }
        let ch        = maybe (New newAcc) (Upd . updAcc) prevAcc

        pure (AccountId txOutAddr ==> ch)

rememberTx :: TxId -> TxWitnessed -> Expansion ctx ChangeEntry
rememberTx txId tw = do
    -- before we can add next changeset entry, we have to check our transaction is new
    -- to the chain
    txAlreadyExists <- queryOneExists txId
    when txAlreadyExists $
        throwLocalError $ TransactionAlreadyExists txId

    return (txId ==> New (TxItself tw))

expandTxHistory :: GTxId -> [Address] -> Expansion ctx [ChangeEntry]
expandTxHistory gTxId txAddrs =
    fmap concat . forM txAddrs $ \txAddr ->
        queryOne (TxsOf txAddr) <&> \case
            Nothing -> [TxsOf txAddr ==> New (LastTx gTxId)]
            Just (LastTx prevTxId) ->
                [ TxsOf  txAddr       ==> Upd (LastTx gTxId)
                , TxHead txAddr gTxId ==> New (TxNext prevTxId)
                ]

expandAddFees
    :: Integer
    -> [TxOut]
    -> Address
    -> Coin
    -> Map Ids (ValueOp Values)
    -> Expansion ctx (Map Ids (ValueOp Values))
expandAddFees inputSent outputs feesReceiver minimalFees changes = do
      -- Adding fees
    let feeAmount =
            max 0 $ inputSent - sum (map (coinToInteger . txOutValue) outputs)
    let feesReceiverAccountId = AccountId feesReceiver
    feesReceiverAccM <- queryOne feesReceiverAccountId
    let addFee receiver = receiver { aBalance = aBalance receiver + feeAmount }
    let onlyFees = Account { aBalance = feeAmount, aNonce = 0 }
    let feesReceiverUpd = maybe (New onlyFees) (Upd . addFee) feesReceiverAccM

    unless (feeAmount >= coinToInteger minimalFees) $
        throwLocalError InsufficientFees
            { aeActualFees = feeAmount
            , aeExpectedFees = coinToInteger minimalFees }

    let changesWithFees
            | feeAmount == 0 = changes
            | otherwise =
            -- Current key we're interested in
            let k = inj feesReceiverAccountId
                updated =
                    Map.adjust (fmap (\(AccountOutVal x) -> AccountOutVal $ addFee x)) k changes
            in case Map.lookup k changes of
                Nothing      -> Map.insert k (inj feesReceiverUpd) changes
                Just (New _) -> updated
                Just (Upd _) -> updated
                Just Rem     -> Map.insert k (New $ inj onlyFees) changes
                other        -> error $ "changesWithFees: not expected: " <> show other

    return changesWithFees

--------------------------------------------------------------------------
-- Block meta tx
--------------------------------------------------------------------------

-- | Expands block meta.
-- Should happend strictly after validation of transactions in block's body.
-- Those checks which happen in block integrity validation are missing here.
seqExpandersBlockMetaTx
    :: HasCoreConfig
    => SeqExpanders Exceptions Ids Proofs Values ctx BlockMetaTx
seqExpandersBlockMetaTx =
    SeqExpanders $ one $ Expander inP outP $ \BlockMetaTx{..} -> do
        let block@Block{ bHeader = header, bBody = body } = bmtBlock
        let issuerAddr = mkAddr (hIssuer header)
        let isGenesis = header == bHeader genesisBlock

        {- Validation -}

        when isGenesis $
            -- further we assume that our genesis block has the least possible difficulty
            unless (hDifficulty header == 0) $
                error "Genesis block difficulty must be 0"

        unless isGenesis $ do
            mSimilarHeaderHash <- queryOne (hDifficulty header)
            case mSimilarHeaderHash of
                Nothing -> pass
                Just similarHeaderHash ->
                    throwLocalError DuplicatedDifficulty
                    { bmeProvidedHeader = header, bmeExistingHeaderHash = similarHeaderHash }

            -- At this point there is no block in chain with exactly the same
            -- header as ours

            prevHash <-
                (hDifficulty header - 1)
                `assertExists` DifficultyIsTooLarge (hDifficulty header)
            (sBlockReconstruct -> Block prevHeader _) <-
                SD.BlockRef prevHash `assertExists` noPrevBlock prevHash

            unless (hPrevHash header == prevHash) $
                  throwLocalError PrevBlockIsIncorrect
                  { bmeProvidedHash = hPrevHash header, bmeTipHash = prevHash }
            unless (hSlotId header > hSlotId prevHeader) $
                  throwLocalError SlotIdIsNotIncreased
                  { bmeProvidedSlotId = hSlotId header, bmeTipSlotId = hSlotId prevHeader }

            let GovCommittee com = gcGovernance $ giveL @CoreConfig
            unless (committeeOwnsSlot com issuerAddr (hSlotId header)) $
                throwLocalError $ IssuerDoesNotOwnSlot
                { bmrSlotId = hSlotId header, bmrIssuer = issuerAddr }

        let signedData = BlockToSign (hDifficulty header) (hSlotId header)
                                     (hPrevHash header) (hash body)
        unless (verify (hIssuer header) signedData (hSignature header)) $
            throwLocalError InvalidBlockSignature

        {- Change sets -}

        let diffChange = Map.singleton
                (inj $ hDifficulty header)
                (New . BlockIdxVal $ headerHash header)

        -- no keys collision is possible because at this point hPrevHash matches tip,
        -- and block at tip is unique
        let forwardBlockChange = Map.singleton
                (inj . NextBlockOf $ hPrevHash header)
                (New . NextBlockOfVal . NextBlock $ headerHash header)

        -- no keys collision is possible sinse we have validated transactions
        -- before this function call, thus every transaction is unique
        let txBlockRefsChange = Map.fromList $
                enumerate (bbTxs (bBody block)) <&> \(idx, gTx) ->
                    ( inj . TxBlockRefId . toGTxId . unGTxWitnessed $ gTx
                    , New . TxBlockVal $ TxBlockRef (headerHash block) idx
                    )

        let totalChange = mconcat [diffChange, forwardBlockChange, txBlockRefsChange]
        pure $ mkDiffCS (totalChange :: Map.Map Ids (ValueOp Values))
  where
    -- Account prefixes are used during the computation to access current balance
    inP  = Set.fromList []
    -- Expander returns account changes only
    outP = Set.fromList [blockIdxPrefix, txBlockPrefix, nextBlockPrefix]

    noPrevBlock (h :: HeaderHash) =
        BlockMetaInternalError $ "Can't resolve previous block " +| h |+ ""

--------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------

(==>)
    :: forall id value k v
    .  HasReview k id
    => HasReview v value
    => id
    -> value
    -> (k, v)
k ==> v = (inj k, inj v)
