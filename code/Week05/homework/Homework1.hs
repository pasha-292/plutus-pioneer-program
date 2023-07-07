{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Homework1 where

import           Data.Maybe                (fromJust)
import           Plutus.V1.Ledger.Interval (contains, overlaps, to, from)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, TxInfo (txInfoValidRange), CurrencySymbol,
                                            MintingPolicy, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx.Builtins         
import qualified PlutusTx                  
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (.), not, (&&))
import           Prelude                   (IO, Show (show))
import           Text.Printf               (printf)
import           Utilities                 (currencySymbol, wrapPolicy,
                                            writeCodeToFile, writePolicyToFile)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy _pkh _deadline () _ctx = ((traceIfFalse "signature missing" signedByBeneficiary) &&
                                (traceIfFalse "deadline not reached" deadlineNotReached))
                                where
                                     info :: TxInfo
                                     info = scriptContextTxInfo _ctx

                                     signedByBeneficiary :: Bool
                                     signedByBeneficiary = txSignedBy info _pkh

                                     deadlineNotReached :: Bool
                                     deadlineNotReached = contains (to $ _deadline) $ txInfoValidRange info

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
