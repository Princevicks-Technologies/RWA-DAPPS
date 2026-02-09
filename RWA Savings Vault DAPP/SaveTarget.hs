{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Prelude as P (IO, FilePath, putStrLn, (++))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import GHC.Generics (Generic)

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

-------------------------------------------------------------------------------
-- DATUM & REDEEMER (VAULT ID)
-------------------------------------------------------------------------------

data Vault = Vault
    { vId        :: BuiltinByteString  -- Unique vault ID
    , vOwner     :: PubKeyHash
    , vTarget    :: Integer
    , vDeposited :: Integer
    }
    deriving Generic

PlutusTx.unstableMakeIsData ''Vault

newtype VaultDatum = VaultDatum
    { vaults :: [Vault] }

PlutusTx.unstableMakeIsData ''VaultDatum

data VaultAction
    = Deposit BuiltinByteString Integer    -- Use vaultId
    | Withdraw BuiltinByteString           -- Use vaultId

PlutusTx.unstableMakeIsData ''VaultAction

-------------------------------------------------------------------------------
-- HELPERS
-------------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE getOnlyContinuing #-}
getOnlyContinuing :: ScriptContext -> TxOut
getOnlyContinuing ctx =
    case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one continuing output"

{-# INLINABLE getContinuingDatum #-}
getContinuingDatum :: ScriptContext -> VaultDatum
getContinuingDatum ctx =
    case txOutDatum (getOnlyContinuing ctx) of
        OutputDatum (Datum d) -> unsafeFromBuiltinData d
        _ -> traceError "expected inline datum"

{-# INLINABLE findVaultById #-}
findVaultById :: BuiltinByteString -> [Vault] -> Vault
findVaultById vaultId vs =
    case filter (\v -> vId v == vaultId) vs of
        [v] -> v
        _   -> traceError "vault not found"

-------------------------------------------------------------------------------
-- VALIDATOR
-------------------------------------------------------------------------------

{-# INLINABLE mkVaultValidator #-}
mkVaultValidator :: VaultDatum -> VaultAction -> ScriptContext -> Bool
mkVaultValidator (VaultDatum oldVaults) action ctx =
    case action of

        Deposit vaultId amount ->
            let oldVault = findVaultById vaultId oldVaults
                newDeposited = vDeposited oldVault + amount

                VaultDatum newVaults = getContinuingDatum ctx
                updatedVault = findVaultById vaultId newVaults
            in
                traceIfFalse "not signed by owner" (signedBy (vOwner oldVault) ctx) &&
                traceIfFalse "deposit must be positive" (amount > 0) &&
                traceIfFalse "deposited amount incorrect" (vDeposited updatedVault == newDeposited)

        Withdraw vaultId ->
            let oldVault = findVaultById vaultId oldVaults
            in
                traceIfFalse "target not reached" (vDeposited oldVault >= vTarget oldVault) &&
                traceIfFalse "must be signed by owner" (signedBy (vOwner oldVault) ctx) &&
                traceIfFalse "no continuing output expected" (null (getContinuingOutputs ctx))

-------------------------------------------------------------------------------
-- UNTYPED WRAPPER
-------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkVaultValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript
        $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------------------------------------
-- FILE OUTPUT FUNCTIONS
-------------------------------------------------------------------------------

-- Write TextEnvelope (.plutus)
writeValidator :: P.FilePath -> Validator -> P.IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    P.putStrLn $ "Validator written to: " P.++ path

-- Write raw CBOR bytes
writeCBOR :: P.FilePath -> Validator -> P.IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
    BS.writeFile path bytes
    P.putStrLn $ "CBOR written to: " P.++ path

-- Write CBOR as hex
writeHex :: P.FilePath -> Validator -> P.IO ()
writeHex path val = do
    let cborBytes = LBS.toStrict (Serialise.serialise val)
        hexBytes  = Base16.encode cborBytes
        hexText   = TE.decodeUtf8 hexBytes
    TIO.writeFile path hexText
    P.putStrLn $ "HEX written to: " P.++ path

-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------

main :: P.IO ()
main = do
    writeValidator "saveTarget.plutus" validator
    writeCBOR   "saveTarget.cbor"   validator
    writeHex    "saveTarget.hex"    validator
    P.putStrLn "Savings Vault validator (.plutus, .cbor, .hex) generated successfully."
