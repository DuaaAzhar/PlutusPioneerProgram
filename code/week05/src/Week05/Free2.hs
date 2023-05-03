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

--mkValidator :: Datum -> Redeemer ->  ScriptContext -> Bool
--like that of mkValidator from validation script
{-#INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

--like that of validator in validation script
{-#INLINABLE policy #-}	
policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])
--policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

--make currency symbol from policy script
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy




-- ........................Offchain code........................

--parameters for minting
data MintParams = MintParams
     { mpTokenName :: !TokenName
     , mpAmount    :: !Integer
     } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- schema to make endpoints
type FreeSchema = Endpoint "mint" MintParams

-- mint contract
mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
     let val = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
         lookups = Constraints.mintingPolicy policy     -- mintingPolicy :: MintingPolicy -> ScriptLookups a
         tx      = Constraints.mustMintValue val        --mustMintValue :: forall i o. Value -> TxConstraints i o
     ledgerTx <- submitTxConstraintsWith @Void lookups tx   -- output = Contract w s e CardanoTx
     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
     Contract.logInfo @String $ printf "forgedTx %s" (show val)
     
--some more boilerplate to define our endpoint, to be able to actually execute the mint function
endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
   where
     mint' = awaitPromise $ endpoint @"mint" mint
     
     
--The final two lines are just needed for the playground UI.
mkSchemaDefinitions ''FreeSchema	
mkKnownCurrencies []
   
   
   
-- ........................Testing with Emulator Trace ........................

test :: IO ()
test = runEmulatorTraceIO $ do
     let tn = "ABC"
     h1 <- activateContractWallet (knownWallet 1) endpoints
     h2 <- activateContractWallet (knownWallet 2) endpoints
     
     callEndpoint @"mint" h1 $ MintParams
         { mpTokenName = tn
         , mpAmount    = 555
         }
     
     callEndpoint @"mint" h2 $ MintParams
         {mpTokenName = tn
         , mpAmount   = 444
         }    
   
     void $ Emulator.waitNSlots 1
     
     callEndpoint @"mint" h1 $ MintParams
         { mpTokenName = tn
         , mpAmount    = -222
         }
     void $ Emulator.waitNSlots 1
     
    
     





