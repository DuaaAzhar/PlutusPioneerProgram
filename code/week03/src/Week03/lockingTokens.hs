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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.LockingTokens where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)
import           Ledger.Value         as Value
import           Data.List.NonEmpty   (NonEmpty (..))

data StakingDatum = StakingDatum
     { staker :: PaymentPubKeyHash
     , deadline :: POSIXTime
     , nft      :: !CurrencySymbol
     , token :: !TokenName
     }
     
PlutusTx.unstableMakeIsData ''StakingDatum

mkValidator :: StakingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "not Signed by staker" signedByStaker    &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx  
  
    signedByStaker :: Bool
    signedByStaker = txSignedBy info $ unPaymentPubKeyHash $ staker dat
    
    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
    
    
data Staking
instance Scripts.ValidatorTypes Staking where
     type instance DatumType Staking = StakingDatum
     type instance RedeemerType Staking = ()
     
typedValidator :: Scripts.TypedValidator Staking
typedValidator = Scripts.mkTypedValidator @Staking
       $$(PlutusTx.compile [||mkValidator ||])
       $$(PlutusTx.compile [|| wrap||])
  where
     wrap = Scripts.wrapValidator @StakingDatum @()
     

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


minLovelace :: Integer
minLovelace = 2000000


data GiveParams = GiveParams
     { gpStaker   :: !PaymentPubKeyHash
     , gpDeadline :: !POSIXTime
     , gpCurrency :: !CurrencySymbol
     , gpToken    :: !TokenName
     } deriving( Generic , ToJSON, FromJSON, ToSchema)

data ULParams = ULParams
     { ulCurrency :: !CurrencySymbol
     , ulToken    :: !TokenName
     } deriving( Generic , ToJSON, FromJSON, ToSchema)     
type StakingSchema = Endpoint "lock" GiveParams
                 .\/ Endpoint "unLock" ULParams
                 
lock :: AsContractError e => GiveParams -> Contract w s e () 
lock gp = do 
     let dat = StakingDatum
                    { staker   = gpStaker gp
                    , deadline = gpDeadline gp
                    , nft      = gpCurrency gp
                    , token    = gpToken gp
                    }
         tokenValue = Value.singleton (nft dat)  (token dat)  (1)           
         v  = tokenValue  <> Ada.lovelaceValueOf minLovelace
         tx = Constraints.mustPayToTheScript dat v
     ledgerTx <- submitTxConstraints typedValidator tx
     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
     logInfo @String $ printf "made the stake of %s lovelace by %s with deadline %s "
          (show $ gpToken gp)
          (show $ gpStaker gp)
          (show $ gpDeadline gp)
             
unLock :: forall w s e. AsContractError e => ULParams -> Contract w s e ()
unLock ul = do
       now <- currentTime
       pkh <- ownPaymentPubKeyHash
       utxos <- Map.filter (issuitable pkh now) <$> utxosAt scrAddress
       if Map.null utxos
          then logInfo @String $ "No Stake amount available"
          else do 
            let orefs = fst <$> Map.toList utxos 
                lookups = Constraints.unspentOutputs utxos <> Constraints.otherScript validator
                tokenValue = Value.singleton (ulCurrency ul)  (ulToken ul)  (1)
                tx      :: TxConstraints Void Void
                tx      = Constraints.mustPayToPubKey pkh (tokenValue <> Ada.lovelaceValueOf minLovelace)  <>
                          mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "Collected Staked Amount"   
 where
    issuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    issuitable pkh now o = case _ciTxOutDatum o of
         Left _            -> False
         Right (Datum e)   -> case PlutusTx.fromBuiltinData e of
            Nothing       -> False
            Just d        -> staker d == pkh && deadline d <= now
          

endpoints :: Contract () StakingSchema Text ()
endpoints = awaitPromise (lock' `select` unLock') >> endpoints
 where
   lock'   = endpoint @"lock" lock
   unLock' = endpoint @"unLock" unLock
  
  
  
myNft1 :: KnownCurrency
myNft1 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T1" :| [])
  
  
myNft2 :: KnownCurrency
myNft2 = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T2" :| [])  
mkSchemaDefinitions ''StakingSchema
mkKnownCurrencies ['myNft1, 'myNft2] 

                   
             
             
             
                   




     
      
     
     
     
   
