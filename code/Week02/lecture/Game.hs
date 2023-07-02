{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Game where

import qualified Plutus.V1.Ledger.Api as Plutus
import           PlutusTx             (unstableMakeIsData)
import           PlutusTx.Prelude     hiding (Bool (..), ifThenElse)
import           Utilities            (wrapValidator)
import           Data.Int             (Int)

data GameDatum = GameDatum
    { verifierAddress :: Plutus.Address
    , numbers         :: (Int, Int, Int)
    , winners         :: [Plutus.PubKeyHash]
    , accumulated     :: Plutus.Value
    } deriving (Generic, Plutus.Eq, Plutus.ToData, Plutus.FromData)

unstableMakeIsData ''GameDatum

{-# INLINABLE guessNumber #-}
guessNumber :: (Int, Int, Int) -> Int -> Bool
guessNumber (n1, n2, n3) guess = guess == n1 || guess == n2 || guess == n3

{-# INLINABLE validateGuess #-}
validateGuess :: GameDatum -> Int -> Bool
validateGuess gameDatum guess =
    let
        correctGuess = guessNumber (numbers gameDatum) guess
        existingWinner = any (\winner -> winner == Plutus.ownPubKeyHash) (winners gameDatum)
    in
        not existingWinner && correctGuess

{-# INLINABLE processGuess #-}
processGuess :: GameDatum -> Int -> Plutus.Value -> GameDatum
processGuess gameDatum guess bet =
    let
        correctGuess = guessNumber (numbers gameDatum) guess
        updatedAccumulated = accumulated gameDatum `Plutus.plus` bet
        updatedWinners = if correctGuess then Plutus.ownPubKeyHash : winners gameDatum else winners gameDatum
    in
        gameDatum { winners = updatedWinners, accumulated = updatedAccumulated }

{-# INLINABLE mkValidator #-}
mkValidator :: GameDatum -> Int -> Plutus.ScriptContext -> Bool
mkValidator gameDatum guess _ =
    if validateGuess gameDatum guess
        then True
        else Plutus.traceIfFalse "Invalid guess" False

wrappedValidator :: Plutus.BuiltinData -> Plutus.BuiltinData -> Plutus.BuiltinData -> ()
wrappedValidator = wrapValidator . mkValidator

gameValidator :: Plutus.Validator
gameValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
