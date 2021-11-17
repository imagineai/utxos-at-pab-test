{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Main ( main ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prettyprinter
import Data.OpenApi.Schema qualified as OpenApi

import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import Playground.Types (FunctionSchema)
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Plutus.PAB.Run (runWith)
import Plutus.PAB.Effects.Contract.Builtin ( HasDefinitions (..)
                                           , SomeBuiltin (..)
                                           )
import Schema (FormSchema)

import CardanoSmartContract

main :: IO ()
main = runWith (Builtin.handleBuiltin @CardanoSmartContract)

data CardanoSmartContract = Init
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty CardanoSmartContract where
    pretty = viaShow

instance HasPSTypes CardanoSmartContract where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @CardanoSmartContract ]

instance HasDefinitions CardanoSmartContract where
    getDefinitions = [ Init ]
    getContract = getExampleContracts
    getSchema = getExampleContractsSchema

getExampleContractsSchema
    :: CardanoSmartContract
    -> [FunctionSchema FormSchema]
getExampleContractsSchema = \case
    Init{} -> Builtin.endpointsToSchemas @BugContractSchema

getExampleContracts :: CardanoSmartContract -> SomeBuiltin
getExampleContracts = \case
    Init -> SomeBuiltin CardanoSmartContract.run
