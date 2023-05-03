{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

tn :: TokenName
tn = TokenName emptyByteString

{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx = traceIfFalse "UTXO not consumed" hasUTXO &&
                       traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTXO :: Bool
    hasUTXO = any (\i -> txInInfoOutRef i==oref) $ txInfoInputs info
    
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
            [(cs, tn' , amt)] -> cs == ownCurrencySymbol ctx && tn'== tn && amt ==1
            _                -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
           $$(PlutusTx.compile [||\oref'-> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
           `PlutusTx.applyCode` PlutusTx.liftCode oref
           

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref =  scriptCurrencySymbol $ policy oref

type NFTSchema = Endpoint "mint" Address

mint :: Address -> Contract w NFTSchema Text ()
mint na = do
     utxos <- utxosAt na
     case Map.keys utxos of
         []       -> Contract.logError @String "No UTXO FOound" 
         oref : _ -> do
            let val    = Value.singleton (curSymbol oref) tn 1 
                lookups= Constraints.mintingPolicy (policy oref) <> Constraints.unspentOutputs utxos
                tx     = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            --Contract.logInfo @String $ printf "%forged %s" (show val)
            Contract.logInfo @String "Forged value here ..."

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ mockWalletAddress w1
    callEndpoint @"mint" h2 $ mockWalletAddress w2
    void $ Emulator.waitNSlots 1
