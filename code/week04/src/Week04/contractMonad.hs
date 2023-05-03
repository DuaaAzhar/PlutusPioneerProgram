{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Week04.Contract2 where

import Control.Monad.Freer.Extras as Extras
import Data.Functor                  (void)
import Data.Text                     (Text, unpack)
import Data.Void                     (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

myContract1 :: Contract () BlockchainActions Text ()
myContract = Contract.logInfo @String "Hello from the contract!"


myTrace1 :: EmulatorTrace ()
myTrace1 = void $activateContractWallet (Wallet 1) myContract1


test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

