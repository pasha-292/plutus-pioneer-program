{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Game where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile)
import           PlutusTx.Builtins    as Builtins (mkI)
import           PlutusTx.Prelude     hiding (&&, (<=), (>=))
import           Prelude              (IO)
import           PlutusTx.Bool        as PlutusBool
import           PlutusTx.Maybe       as PlutusMaybe
import           PlutusTx.Eq          as PlutusEq
import           PlutusTx.List        as PlutusList
import           Utilities            (writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- This validator succeeds only if the redeemer contains three numbers between 0 and 100
--                  Datum         Redeemer     ScriptContext
gameValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
gameValidator _ r _
    | isMatch r = ()
    | otherwise = traceError "expected three numbers between 0 and 100"
  where
    isMatch :: BuiltinData -> Bool
    isMatch redeemer =
        case redeemer of
            [x, y, z] -> isValidNumber x && isValidNumber y && isValidNumber z
            _         -> False

    isValidNumber :: BuiltinData -> Bool
    isValidNumber n = n PlutusEq.>= Builtins.mkI 0 && n PlutusEq.<= Builtins.mkI 100
{-# INLINABLE gameValidator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| gameValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/game.plutus" validator
