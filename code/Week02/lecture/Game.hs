{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Game where

import           Plutus.V2.Ledger.Api      (ScriptContext (scriptContextTxInfo), PubKeyHash, Validator, mkValidatorScript, adaToken, adaSymbol, singleton)
import           Plutus.V2.Ledger.Contexts (valuePaidTo, TxInfo)
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Builtins         (BuiltinData, Integer)
import           PlutusTx.Prelude          (Bool (..), (==), traceIfFalse, (||))
import           Utilities                 (wrapValidator)


---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data GameDatum = GameDatum
  { number1 :: Integer
  , number2 :: Integer
  , number3 :: Integer
  }
PlutusTx.unstableMakeIsData ''GameDatum

{-# INLINABLE mkValidator #-}
mkValidator :: GameDatum -> Integer -> ScriptContext -> Bool
mkValidator ds r ctx = traceIfFalse "Your number does not match" numbersMatch
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        n1 = number1 ds
        n2 = number2 ds
        n3 = number3 ds

        numbersMatch :: Bool
        numbersMatch = (r == n1) || (r == n2) || (r == n3)


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator


validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])
