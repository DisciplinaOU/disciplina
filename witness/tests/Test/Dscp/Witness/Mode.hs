module Test.Dscp.Witness.Mode
    ( WitnessTestMode'
    , witnessProperty
    ) where

import Control.Lens (makeLenses)
import Loot.Log (Logging (..))

import Dscp.DB.CanProvideDB as DB
import Dscp.DB.CanProvideDB.Pure as PureDB
import Dscp.Resource.Keys
import Dscp.Rio
import Dscp.Util
import Dscp.Util.HasLens
import Dscp.Util.Test
import Dscp.Witness

----------------------------------------------------------------------------
-- Test witness mode
----------------------------------------------------------------------------

data TestWitnessCtx = TestWitnessCtx
    { _twcVars    :: WitnessVariables
    , _twcLogging :: Logging IO
    , _twcKeys    :: KeyResources WitnessNode
    , _twcDb      :: DB.Plugin
    }

makeLenses ''TestWitnessCtx
deriveHasLensDirect ''TestWitnessCtx

deriveHasLens 'twcVars ''TestWitnessCtx ''WitnessVariables

type WitnessTestMode' = RIO TestWitnessCtx

----------------------------------------------------------------------------
-- Runner
----------------------------------------------------------------------------

runWitnessTestMode :: WitnessTestMode' a -> IO a
runWitnessTestMode action =
    withWitnessConfig testWitnessConfig $ runRIO testLogging $ do
        _twcKeys <- mkCommitteeStore (CommitteeParamsOpen 0)
        _twcDb   <- PureDB.plugin <$> liftIO PureDB.newCtxVar
        _twcVars <- mkTestWitnessVariables (_twcKeys ^. krPublicKey) _twcDb
        let _twcLogging = testLogging
        let ctx = TestWitnessCtx{..}

        runRIO ctx $ do
            markWithinWriteSDLockUnsafe applyGenesisBlock
            action

witnessProperty
    :: Testable prop
    => ((HasWitnessConfig, WithinWriteSDLock) => PropertyM WitnessTestMode' prop)
    -> Property
witnessProperty action =
    -- Note on 'execUnmasked': for some reason, tests are run under 'mask'.
    -- Snowdrop expects it to be not like that, for instance `mappend` on
    -- snowdrop actions, which is used in validation, uses 'concurrently' under
    -- hood, which may hang when is executed under 'mask'.
    monadic (ioProperty . execUnmasked . runWitnessTestMode) $ do
        prop <- markWithinWriteSDLockUnsafe $
                withWitnessConfig testWitnessConfig action
        void $ stop prop
