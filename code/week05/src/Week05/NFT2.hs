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

module Week05.NFT2 where

import           Control.Monad          hiding (fmap)   -- to use void 
import		 Ledger.Value    	as Value	 -- to use CurrencySymbol
import		 Ledger.Contexts   			 -- to use ScriptContext
import		 Ledger.Scripts    
import 	 Ledger.Typed.Scripts   as Scripts  	 -- to write validation scripts like Scripts.MintingPolicy
import           Plutus.Contract        as Contract  	 -- to use Contract w s e a
import qualified PlutusTx  				 --to use PlutusTx.Compile
import           PlutusTx.Bool            		 --to use bool
import  	 Data.Text              (Text) 		 --to use Text
import           Prelude                (IO, Show (..), String) --to use String
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)   --for deriving ToSchema etc
import           Ledger.Constraints     as Constraints   -- to use Constraints 
import 	 GHC.Generics           		  --deriving Generic
import 	 Data.Aeson             		  -- deriving ToJSON, fromJSON
import           Data.Void              (Void)       	  -- to use Void
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)    -- to use Integer
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)    --}--|>====
import           Playground.Types       (KnownCurrency (..))                        --}--|>====both of these for mkKnownCurrencies
import           Text.Printf            (printf)
import 	 Ledger.Tx              hiding (mint)               --to use getCardanoTxId
import           Ledger                 hiding (mint, singleton)    -- to avoid duplication of mint in Plutus library and this contract
import 	Plutus.Trace.Emulator as Emulator                   -- to use Emulator.waitNSlots 
import 	 Wallet.Emulator.Wallet                             -- to use knownWallet
import qualified Data.Map               as Map -- To use Map.keys 
import           Prelude                (Semigroup (..))  -- to use <> semigroup operator
-- -----------------------------------OnChain-------------------------------------------------------

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTXO not consumed" hasUTXO &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
   where
        info :: TxInfo
        info = scriptContextTxInfo ctx
   
        hasUTXO :: Bool
        hasUTXO = any (\i -> txInInfoOutRef i== oref) $ txInfoInputs info
   
        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
                [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn'==tn  && amt == 1
                _                -> False
                
-- Boiler plate for compilation
policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
   `PlutusTx.applyCode` PlutusTx.liftCode oref
   `PlutusTx.applyCode` PlutusTx.liftCode tn 

--currency symbol
curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn


-- -------------------------------OffChain---------------------------------------------------
data NFTParams = NFTParams
    { npToken     :: !TokenName
    , npAddress   :: !Address
    } deriving (Generic, ToJSON , FromJSON , Show)

-- schema for endpoints 
type NFTSchema = Endpoint "mint" NFTParams

-- mint endpoint contract

--To filter the UTXOS--- utxosAt :: AsContractError e => Address -> Contract w s e (Data.Map.Internal.Map TxOutRef ChainIndexTxOut)

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do 
	    utxos <- utxosAt (npAddress np)
    case Map.keys utxos of
         []       ->  Contract.logError @String "No UTXO Found !!"
         oref : _ -> do
             let tn      = npToken np
             let val     = Value.singleton (curSymbol oref tn) tn 1
                 --to provide lookup access to where the UTxO oref can be found
                 --Ledger.Constraints.unspentOutputs :: Data.Map.Internal.Map TxOutRef TxOutTx -> ScriptLookups a
                 lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos   -- <>  = semiGroup operator
                 tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
             ledgerTx <- submitTxConstraintsWith @Void lookups tx
             void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
             --Contract.logInfo @String $ printf "forged %s" (show val)
             Contract.logInfo @String $ printf "forged %s" (show val)
             
             
endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
   where
     mint' = awaitPromise $ endpoint @"mint" mint 
                 

  
  
  
test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    
    callEndpoint @"mint" h1 NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress (knownWallet 1)
        }
    
    callEndpoint @"mint" h2 NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress (knownWallet 2) 
        }    
        
    void $ Emulator.waitNSlots 1

    


   





