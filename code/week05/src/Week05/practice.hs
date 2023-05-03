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




-- Onchain part----

mkPolicy :: () -> ScriptContext -> Bool
mkPolicy ()_ = True

policy :: Scripts.MintingPolicy  
policy = mkMintingPolicyScript $$ (Plutus.compile $$ [||Scripts.wrapMintingPolicy mkPolicy ||])


curSymbol :: CurSymbol
curSymbol = scriptCurrencySymbol policy
