module Main (main) where

import Week06.Token.OnChain (tokenPolicy)
import Week06.Utils (unsafeReadTxOutRef, writeMintingPolicy)

main :: IO ()
main = do
  [file, oref', amt', tn'] <- getArgs
  let oref = unsafeReadTxOutRef oref'
      tn   = read tn'
      amt  = fromString amt'
      p    = tokenPolicy oref tn amt 
      e   <- writeMintingPolicy file p
      case e of
          Left err -> throwIO $ userError $ show err
          Right () -> return ()
