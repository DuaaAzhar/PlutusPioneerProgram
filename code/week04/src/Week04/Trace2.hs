{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace2 where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Types (Contract (..))
import Data.Text


import Week04.Vesting

myTrace :: EmulatorTrace()
myTrace = do
   h1 <- activateContractWallet (knownWallet 1) endpoints
   h2 <- activateContractWallet (knownWallet 2) endpoints
   callEndpoint @"give" h1 $GiveParams
      {gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
      , gpDeadline   = slotToBeginPOSIXTime def 20
      , gpAmount     =10000000
      }
   void $ waitUntilSlot 20
   callEndpoint @"grab" h2 ()
   s <- waitNSlots 2
   Extras.logInfo $ "reached" ++ show s



test :: IO ()
test = runEmulatorTraceIO myTrace


-- .....................Contract Monads.......................

myContract1 :: Contract () BlockchainActions Text ()
myContract = Extras.logInfo @String "Hello from the contract!"


myTrace1 :: EmulatorTrace ()
myTrace1 = void $activateContractWallet (Wallet 1) myContract1


test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1


