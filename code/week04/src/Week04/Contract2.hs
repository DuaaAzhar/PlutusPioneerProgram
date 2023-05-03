{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract2 where

import Control.Monad.Freer.Extras as Extras
import Data.Functor                  (void)
import Data.Text                     (Text, unpack)
import Data.Void                     (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
-- Hello from contract with an error msg
myContract1 :: Contract () Empty Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "Hello from the contract!" 

myTrace1 :: EmulatorTrace ()
myTrace1 = void $activateContractWallet (knownWallet 1) myContract1


test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

--Handle error contract
myContract2 :: Contract () Empty Void ()
myContract2 = do
    Contract.handleError (\err -> Contract.logError $ "Caught Error " ++ unpack err) myContract1


myTrace2 :: EmulatorTrace ()
myTrace2 = void $activateContractWallet (knownWallet 1) myContract2


test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2



-- Including the 2nd param that is BlockchainActions

type MySchema =  Endpoint "foo" Int

--The operator .\/ is a type operator - it operates on types, not values. In order to use this we need to use the TypeOperators and DataKinds compiler options.

--Now using the mySchema type to define our contract
myContract3 :: Contract () MySchema Text ()
myContract3 = do
     awaitPromise $ endpoint @"foo" Contract.logInfo
     
myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (knownWallet 1) myContract3
    callEndpoint @"foo" h 42
    
test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3


-- using the first param i.e. writer w

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
     void $ Contract.waitNSlots 10
     tell [1]
     void $ Contract.waitNSlots 10
     tell [2]
     void $ Contract.waitNSlots 10
     
     
myTrace4 :: EmulatorTrace ()
myTrace4 = do
     h <- activateContractWallet (knownWallet 1) myContract4
      
     void $ Emulator.waitNSlots 5
     xs <- observableState h
     Extras.logInfo $ show xs
     
     void $ Emulator.waitNSlots 10
     ys <- observableState h
     Extras.logInfo $ show ys
     
     void $ Emulator.waitNSlots 10
     zs <- observableState h
     Extras.logInfo $ show zs
   
test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4



