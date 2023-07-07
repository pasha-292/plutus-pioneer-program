{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Game where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile)
import           PlutusTx.Prelude     (otherwise, traceError, (==), Bool)
import           Prelude              (IO, Int, Bool)
import           Utilities            (writeValidatorToFile)

---------------------------------------------------------------------------------------------------
-------------------------------- ON-CHAIN CODE / VALIDATOR ----------------------------------------

-- This validator checks if the guess is correct and returns the winner address
--                    Datum         Redeemer     ScriptContext
validateGuess :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validateGuess _ redeemer _ =
  let
    secretNumbers = [12, 34, 56]
  in
    if isMember redeemer secretNumbers then
      ()
    else
      traceError "expected a different number"
{-# INLINABLE validateGuess #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| validateGuess ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

isMember :: Int -> [Int] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/game.plutus" validator

main :: IO ()
main = do
  saveVal