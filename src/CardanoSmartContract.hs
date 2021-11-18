{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module CardanoSmartContract where

import           Control.Monad             (forever)
import           Control.Lens
import           Data.Text
import qualified Data.Map          as Map
import qualified Prelude as P

import           PlutusTx.Prelude
import qualified PlutusTx
import           Ledger.Constraints        as Constraints
import           Plutus.Contract           as Contract hiding (tell)
import           Plutus.V1.Ledger.Ada      (lovelaceValueOf)
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts

-- OnChain Code

-- We implemented the validator like this, so changing the number we quickly get
-- a different script-address.
{-# INLINABLE mkContractValidator #-}
mkContractValidator
    :: ()
    -> ()
    -> ScriptContext
    -> Bool
mkContractValidator _ _ _ = (\x -> x==x) (42 :: Integer)

-- Validator boilerplate
data Contracting
instance Scripts.ValidatorTypes Contracting where
    type instance DatumType Contracting    = ()
    type instance RedeemerType Contracting = ()

contractInst :: Scripts.TypedValidator Contracting
contractInst = Scripts.mkTypedValidator @Contracting
               $$(PlutusTx.compile [|| mkContractValidator ||])
               $$(PlutusTx.compile [|| Scripts.wrapValidator ||])

contractValidator :: Validator
contractValidator = Scripts.validatorScript contractInst

contractAddress :: Ledger.Address
contractAddress = scriptAddress contractValidator

-- OffChain Code
run :: Contract () BugContractSchema Text ()
run = start >> endpoints

type BugContractSchema = Endpoint "utxos" ()
                     .\/ Endpoint "spend" ()
                     .\/ Endpoint "spend-pay" ()

endpoints :: Contract () BugContractSchema Text ()
endpoints = forever $ handleError logError $ awaitPromise $
            eopUtxos       `select`
            eopSpend       `select`
            eopSpendAndPay
  where
    eopUtxos :: Promise () BugContractSchema Text ()
    eopUtxos = endpoint @"utxos" $ const opUtxos

    eopSpend :: Promise () BugContractSchema Text ()
    eopSpend = endpoint @"spend" $ const opSpend

    eopSpendAndPay :: Promise () BugContractSchema Text ()
    eopSpendAndPay = endpoint @"spend-pay" $ const opSpendAndPay

start :: Contract () s Text ()
start = do
    logInfo @P.String $ "Starting contract."

    let lkp =    Constraints.typedValidatorLookups contractInst
            P.<> Constraints.otherScript contractValidator

        tx  =    Constraints.mustPayToTheScript () (lovelaceValueOf 3000000)
            P.<> Constraints.mustIncludeDatum builtinDatum

    _ <- submitTxConstraintsWith @Contracting lkp tx

    logInfo @P.String $ "Contract started."
    logInfo @P.String $ "Addr: " ++ P.show contractAddress

opUtxos :: Contract () s Text ()
opUtxos = do
    logInfo @P.String "Calling utxos endpoint."
    utxos <- utxosAt contractAddress
    logInfo @P.String $ "Got utxos: " ++ P.show utxos

opSpend :: Contract () s Text ()
opSpend = do
    logInfo @P.String "Calling spend endpoint."
    pkh <- Contract.ownPubKeyHash
    utxos <- utxosAt contractAddress
    (oref, o) <- case Map.toList utxos of
                    [(oref, o)] -> return (oref, o)
                    _           -> throwError "Unexpected number of UTxOs."

    let scriptValue = o ^. ciTxOutValue
        lookups =    Constraints.ownPubKeyHash pkh
                P.<> Constraints.unspentOutputs utxos
                P.<> Constraints.typedValidatorLookups contractInst
                P.<> Constraints.otherScript contractValidator

        tx =    Constraints.mustSpendScriptOutput oref buildInRedeemer
           P.<> Constraints.mustPayToPubKey pkh scriptValue

    _ <- submitTxConstraintsWith @Contracting lookups tx

    logInfo @P.String $ "Operation spend submitted successfully."

opSpendAndPay :: Contract () s Text ()
opSpendAndPay = do
    logInfo @P.String $ "Calling spend and pay again endpoint."
    utxos <- utxosAt contractAddress
    (oref, o) <- case Map.toList utxos of
                    [(oref, o)] -> return (oref, o)
                    _           -> throwError "Unexpected number of UTxOs."

    let scriptValue = o ^. ciTxOutValue
        lookups = Constraints.unspentOutputs utxos
             P.<> Constraints.typedValidatorLookups contractInst
             P.<> Constraints.otherScript contractValidator

        tx =  Constraints.mustSpendScriptOutput oref buildInRedeemer
         P.<> Constraints.mustPayToTheScript () scriptValue

    _ <- submitTxConstraintsWith @Contracting lookups tx

    logInfo @P.String $ "Operation spend and pay again submitted successfully."

builtinDatum :: Datum
builtinDatum = Datum $ PlutusTx.toBuiltinData ()

buildInRedeemer :: Redeemer
buildInRedeemer = Redeemer $ PlutusTx.toBuiltinData ()
