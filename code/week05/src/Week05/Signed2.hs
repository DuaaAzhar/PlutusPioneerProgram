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

module Week05.Free2 where
import           Control.Monad          hiding (fmap)   -- to use void 
import		 Ledger.Value    	as Value	 -- to use CurrencySymbol
import		 Ledger.Contexts   			 -- to use ScriptContext
import		 Ledger.Scripts    
import 	 Ledger.Typed.Scripts   as Scripts  	 -- to write validation scripts like Scripts.MintingPolicy
import           Plutus.Contract        as Contract  	 -- to use Contract w s e a
import qualified PlutusTx  				 --to use PlutusTx.Compile
import           PlutusTx.Bool            		 --to use bool
import  	 Data.Text               		 --to use Text
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


-- ---------------------------------Onchain -----------------------------

mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
--scriptContextTxInfo :: ScriptContext -> TxInfo
--txSignedBy :: TxInfo -> PubKeyHash -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) (unPaymentPubKeyHash pkh) 


policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||]) 	
    `PlutusTx.applyCode` 
     PlutusTx.liftCode pkh


curSymbol :: PaymentPubKeyHash -> CurrencySymbol
--curSymbol pkh = scriptCurrencySymbol $ policy pkh
 --    OR 
curSymbol = scriptCurrencySymbol . policy   -- in this we are combining the two functions so no need to pass pkh separately
-- it is also called ETA reduction 



-- ----------------------------------OffChain-------------------------------
data MintParams = MintParams
     { mpTokenName :: !TokenName
     , mpAmount    :: !Integer
     } deriving (ToJSON, FromJSON, Generic, ToSchema)


type SignedSchema = Endpoint "mint" MintParams

--to find the pkh in order to pass that to currSymbol, we can use two ways
-- pk <- Contract.ownPubKey
-- pkh <- pubKeyHash pk

--Using the fmap we can use also
-- pkh <- fmap pubKeyHash Contract.ownPubKey
-- pkh <- pubKeyHash <$> Contract.ownPubKey  -- <$> is the sign of fmap


mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do 
     pkh <- Contract.ownPaymentPubKeyHash
     let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
         lookups = Constraints.mintingPolicy $ policy pkh
         tx      = mustMintValue val
     ledgerTx  <-  submitTxConstraintsWith @Void lookups tx 
     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
     Contract.logInfo @String $ printf "forged %s" (show val)
     
endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
   where
     mint'= awaitPromise $ endpoint @"mint" mint
     
mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []



test :: IO ()
test = runEmulatorTraceIO $ do
    let tn= "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1

















