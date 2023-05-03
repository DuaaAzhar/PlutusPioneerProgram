{-# LANGUAGE TypeApplications #-}      -- error at writeFileTextEnvelope @(PlutusScript PlutusScriptV1)


module Week06.Utils 
  ( unsafeReadTxOutRef , writeMintingPolicy , unsafeTokenNameToHex, getCredentials)
   where
import           Plutus.V1.Ledger.Credential as Plutus   
import           Plutus.V1.Ledger.Crypto     as Plutus
import qualified Ledger                      as Plutus
import           Data.String                 (IsString (..))
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Codec.Serialise             (serialise)
import           Data.Text                   (pack)
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))  -- to use 
import           Cardano.Api                 as API      -- to use PlutusScriptV1
import qualified Data.ByteString.Char8       as BS8    -- to use BS8
import           Plutus.V1.Ledger.Value      (TokenName (..)) -- to get TokenName
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))  --get BuiltInByteString
import           Data.Maybe                  (fromJust, fromMaybe)    -- to use fromJust


unsafeReadTxOutRef :: String -> Plutus.TxOutRef
unsafeReadTxOutRef s = 
      let 
        (x , _ : y)= span (/= '#') s
      in 
        Plutus.TxOutRef
          {  Plutus.txOutRefId = fromString x
           , Plutus.txOutRefIdx= read y
          } 

writeMintingPolicy ::  FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy

unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BS8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
    where 
       getByteString (BuiltinByteString bs) = bs
       
-- helper Function for offchain code       
getCredentials :: Plutus.Address -> Maybe (Plutus.PaymentPubKeyHash , Maybe Plutus.StakePubKeyHash)
getCredentials (Plutus.Address x  y) = case x of 
    ScriptCredential _ -> Nothing
    PubKeyCredential pkh -> 
       let ppkh = Plutus.PaymentPubKeyHash pkh
       in 
         case y of
             Nothing -> Just (ppkh , Nothing)
             Just (Plutus.StakingPtr _ _ _) -> Nothing
             Just (StakingHash h )          -> case h of 
                 ScriptCredential _         -> Nothing
                 PubKeyCredential pkh'      -> Just (ppkh , Just $ Plutus.StakePubKeyHash pkh')
       
    
      
