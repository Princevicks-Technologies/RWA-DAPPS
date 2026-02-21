{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Prelude as P
import Prelude (IO, Either(..), Maybe(..), (<>), error)
import qualified System.Directory as Dir


import GHC.Generics (Generic)

-- plutus-apps (2022) imports
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V1.Ledger.Value (adaSymbol, adaToken, valueOf)
import Plutus.V1.Ledger.Address (toPubKeyHash)

import Plutus.V2.Ledger.Api
  ( BuiltinData
  , POSIXTime
  , PubKeyHash(..)
  , Validator
  , Script
  , Value
  , Datum(..)
  , OutputDatum(..)
  , TxOut(..)
  , TxInInfo(..)
  , TxInfo(..)
  , ScriptContext(..)
  , mkValidatorScript
  , unValidatorScript
  )

import Plutus.V2.Ledger.Contexts
  ( txSignedBy
  , findOwnInput
  , getContinuingOutputs
  )

import PlutusTx (applyCode, compile, liftCode, unsafeFromBuiltinData, toBuiltinData)
import qualified PlutusTx

import PlutusTx.Prelude
  ( Bool(..), Integer
  , (==), (<), (>), (>=), (+), (-), (*)
  , ($), (&&), (||)
  , BuiltinByteString
  , foldl
  , traceIfFalse, traceError
  , divide
  , mempty
  )

-- compile tooling
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as BSC
import Codec.Serialise (serialise)

import Cardano.Api (writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

import PlutusTx.Builtins (toBuiltin)

-- =========================================================
-- TYPES
-- =========================================================

data VaultDatum = VaultDatum
  { vdOwner       :: PubKeyHash
  , vdBeneficiary :: BuiltinByteString
  , vdMode        :: Integer  -- 0 time, 1 target, 2 hybrid
  , vdPolicy      :: Integer  -- 0 strict, 1 non-strict
  , vdTarget      :: Integer
  , vdUnlockTime  :: POSIXTime
  }
  deriving (Generic)

data VaultAction = Deposit | Withdraw | ForceWithdraw
  deriving (Generic)

PlutusTx.unstableMakeIsData ''VaultDatum
PlutusTx.unstableMakeIsData ''VaultAction

-- =========================================================
-- CONSTANTS
-- =========================================================

{-# INLINABLE lovelacePerAda #-}
lovelacePerAda :: Integer
lovelacePerAda = 1000000

{-# INLINABLE feeSlack #-}
feeSlack :: Integer
feeSlack = 2000000  -- 2 ADA

{-# INLINABLE t50 #-}
t50 :: Integer
t50 = 50 * lovelacePerAda

{-# INLINABLE t1000 #-}
t1000 :: Integer
t1000 = 1000 * lovelacePerAda

{-# INLINABLE t5000 #-}
t5000 :: Integer
t5000 = 5000 * lovelacePerAda

{-# INLINABLE t10000 #-}
t10000 :: Integer
t10000 = 10000 * lovelacePerAda

{-# INLINABLE fee1 #-}
fee1 :: Integer
fee1 = 1 * lovelacePerAda

{-# INLINABLE fee3 #-}
fee3 :: Integer
fee3 = 3 * lovelacePerAda

{-# INLINABLE fee5 #-}
fee5 :: Integer
fee5 = 5 * lovelacePerAda

{-# INLINABLE fee10 #-}
fee10 :: Integer
fee10 = 10 * lovelacePerAda

-- =========================================================
-- HELPERS
-- =========================================================

{-# INLINABLE lovelaceOf #-}
lovelaceOf :: Value -> Integer
lovelaceOf v = valueOf v adaSymbol adaToken

{-# INLINABLE bytesEmpty #-}
bytesEmpty :: BuiltinByteString -> Bool
bytesEmpty b = b == mempty

{-# INLINABLE recipientPkh #-}
recipientPkh :: VaultDatum -> PubKeyHash
recipientPkh d =
  if bytesEmpty (vdBeneficiary d)
     then vdOwner d
     else PubKeyHash (vdBeneficiary d)

{-# INLINABLE valuePaidToPkhLovelace #-}
valuePaidToPkhLovelace :: TxInfo -> PubKeyHash -> Integer
valuePaidToPkhLovelace info pkh =
  foldl
    (\acc o ->
      case toPubKeyHash (txOutAddress o) of
        Just pkh' ->
          if pkh' == pkh
            then acc + lovelaceOf (txOutValue o)
            else acc
        Nothing -> acc
    )
    0
    (txInfoOutputs info)

{-# INLINABLE mustFindOwnInput #-}
mustFindOwnInput :: ScriptContext -> TxInInfo
mustFindOwnInput ctx =
  case findOwnInput ctx of
    Nothing -> traceError "NO_OWN_INPUT"
    Just i  -> i

{-# INLINABLE mustSingleContinuing #-}
mustSingleContinuing :: ScriptContext -> TxOut
mustSingleContinuing ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "BAD_CONTINUING_OUTPUTS"

{-# INLINABLE noContinuing #-}
noContinuing :: ScriptContext -> Bool
noContinuing ctx =
  case getContinuingOutputs ctx of
    [] -> True
    _  -> False

{-# INLINABLE inlineDatumEquals #-}
inlineDatumEquals :: VaultDatum -> TxOut -> Bool
inlineDatumEquals d o =
  case txOutDatum o of
    OutputDatum (Datum bd) -> bd == toBuiltinData d
    _ -> False

-- =========================================================
-- LOGIC
-- =========================================================

{-# INLINABLE saneDatum #-}
saneDatum :: VaultDatum -> Bool
saneDatum d =
  traceIfFalse "BAD_MODE" modeOk &&
  traceIfFalse "BAD_POLICY" policyOk &&
  traceIfFalse "BAD_FIELDS" fieldsOk
 where
  m = vdMode d
  p = vdPolicy d
  modeOk = (m == 0) || (m == 1) || (m == 2)
  policyOk = (p == 0) || (p == 1)
  fieldsOk =
    if m == 0 then vdUnlockTime d > 0
    else if m == 1 then vdTarget d > 0
    else (vdUnlockTime d > 0) && (vdTarget d > 0)

{-# INLINABLE depositFee #-}
depositFee :: Integer -> Integer
depositFee delta =
  if delta < t50 then 0
  else if delta < t1000 then fee1
  else if delta < t5000 then fee3
  else if delta < t10000 then fee5
  else fee10

{-# INLINABLE unlockOk #-}
unlockOk :: VaultDatum -> Integer -> TxInfo -> Bool
unlockOk d bal info =
  if m == 0 then timeOk
  else if m == 1 then targetOk
  else timeOk && targetOk
 where
  m = vdMode d
  timeOk   = contains (from (vdUnlockTime d)) (txInfoValidRange info)
  targetOk = bal >= vdTarget d

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> Integer -> VaultDatum -> VaultAction -> ScriptContext -> Bool
mkValidator treasuryPkh penaltyBps datum action ctx =
  traceIfFalse "BAD_DATUM" (saneDatum datum) &&
  case action of
    Deposit       -> validateDeposit
    Withdraw      -> validateWithdraw
    ForceWithdraw -> validateForce
 where
  info = scriptContextTxInfo ctx
  ownIn = mustFindOwnInput ctx
  inBal = lovelaceOf (txOutValue (txInInfoResolved ownIn))

  recip = recipientPkh datum
  paidTreas = valuePaidToPkhLovelace info treasuryPkh
  paidRecip = valuePaidToPkhLovelace info recip

  validateDeposit :: Bool
  validateDeposit =
    let out = mustSingleContinuing ctx
        outBal = lovelaceOf (txOutValue out)
        delta = outBal - inBal
        reqFee = depositFee delta
    in traceIfFalse "DELTA<=0" (delta > 0) &&
       traceIfFalse "DATUM_CHANGED" (inlineDatumEquals datum out) &&
       traceIfFalse "FEE_UNPAID" (paidTreas >= reqFee)

  validateWithdraw :: Bool
  validateWithdraw =
    traceIfFalse "NOT_SIGNED" (txSignedBy info (vdOwner datum)) &&
    traceIfFalse "CONT_OUTPUT" (noContinuing ctx) &&
    traceIfFalse "LOCKED" (unlockOk datum inBal info) &&
    traceIfFalse "NOT_PAID" (paidRecip >= (inBal - feeSlack))

  validateForce :: Bool
  validateForce =
    let penalty = (inBal * penaltyBps) `divide` 10000
    in traceIfFalse "NOT_SIGNED" (txSignedBy info (vdOwner datum)) &&
       traceIfFalse "STRICT" (vdPolicy datum == 1) &&
       traceIfFalse "CONT_OUTPUT" (noContinuing ctx) &&
       traceIfFalse "PENALTY" (paidTreas >= penalty) &&
       traceIfFalse "NOT_PAID" (paidRecip >= (inBal - penalty - feeSlack))

-- =========================================================
-- UN-TYPED (AVOIDS TH STAGE RESTRICTION)
-- =========================================================

{-# INLINABLE untyped #-}
untyped :: PubKeyHash -> Integer -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untyped t p d r c =
  if mkValidator t p (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)
  then ()
  else traceError "FAIL"

-- =========================================================
-- COMPILE
-- =========================================================

penaltyBpsParam :: Integer
penaltyBpsParam = 300

treasuryPkhParam :: PubKeyHash
treasuryPkhParam =
  case B16.decode (BSC.pack "33e843bd262a6db8636b718fbb7c805abe840c9d0742490075ab9729") of
    Left err -> error ("Bad hex: " <> err)
    Right b  -> PubKeyHash (toBuiltin b)

validator :: Validator
validator =
  mkValidatorScript $
    $$(compile [|| untyped ||])
      `applyCode` liftCode treasuryPkhParam
      `applyCode` liftCode penaltyBpsParam

script :: Script
script = unValidatorScript validator

scriptCbor :: BS.ByteString
scriptCbor = LBS.toStrict (serialise script)

scriptHex :: BS.ByteString
scriptHex  = B16.encode scriptCbor

main :: IO ()
main = do
  Dir.createDirectoryIfMissing True "artifacts"

  BS.writeFile "artifacts/validator.cbor" scriptCbor
  BS.writeFile "artifacts/validator.hex" scriptHex

  let plutus = PlutusScriptSerialised (SBS.toShort scriptCbor) :: PlutusScript PlutusScriptV2
  _ <- writeFileTextEnvelope "artifacts/validator.plutus" Nothing plutus
  P.pure ()

