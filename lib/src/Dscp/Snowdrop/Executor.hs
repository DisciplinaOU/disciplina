-- | Snowdrop executor.

module Dscp.Snowdrop.Executor () where

--
--demo :: DemoConfiguration -> IO ()
--demo demoConf =
--    avlInitState <- initAVLPureStorage @Ids @Values (inj utxo)
--    (serverStDba, serverLookupHash) <- avlServerDbActions avlInitState
--    serverBlkDba <- blockDbActions
--    let retrieveF :: Hash -> IO (Maybe ByteString)
--        retrieveF h = serverLookupHash h >>= \resp -> do
--          whenJust resp $ \resp' -> do
--            (respV :: AVL.MapLayer Hash Ids Values Hash) <- deserialiseM resp'
--            let respProof = AVL.Proof . Free $ pure <$> respV
--            when (not $ AVL.checkProof h respProof) $ throwM BrokenProofError
--          pure resp
--    let (rootHash :: RootHash) = gett avlInitState
--    clientBlkDba <- blockDbActions
--    clientStDba <- avlClientDbActions retrieveF rootHash
--    let serverNC remForProof = NodeConf (serverStDba remForProof) serverBlkDba blockPK
--    let clientNC clientMode = NodeConf (clientStDba clientMode) clientBlkDba blockPK
--    continue serverNC clientNC
--  where
--    GenUtxo utxo pks = generateDo arbitrary
--    blockSK = generateDo arbitrary
--    blockPK = toPublicKey blockSK
--
--    continue :: forall sCA1 bCA1 proof sCA2 bCA2 .
--                (Buildable proof, Default bCA1, Default bCA2, Default sCA1, Default sCA2) =>
--                (RememberForProof -> NodeConf sCA1 bCA1 proof) ->
--                (ClientMode proof -> NodeConf sCA2 bCA2 ()) ->
--                IO ()
--    continue serverNc clientNc = do
--        sc <- ServerConf serverNc blockSK <$> newTVarIO [] <*> newTVarIO Nothing
--        let addToMempool tx = atomically $ modifyTVar' (scMempool sc) (++ [tx])
--            tipBlock = readTVar (scTipBlock sc)
--            sa = ServerActions addToMempool tipBlock
--        cc <- ClientConf clientNc sa <$> newEmptyMVar
--        putTextLn $ "Initial UTXO"
--        printUtxo $ serverNc $ RememberForProof False
--        putTextLn ""
--        putTextLn $ "Private keys"
--        printPks pks
--        let sourceDba = dmaAccessActions $ nsStateDBActions $ serverNc $ RememberForProof False
--        race_ (race_ (server demoConf sc) (client demoConf cc))
--              (source demoConf sourceDba pks (ccIncomingTx cc))
