#!/bin/bash

# declaring variables
oref=$1
amt=$2
tn=$3
addrFile=$4
skeyFile=$5


# print their values

echo "oref: $oref"
echo "amt: $amt"
echo "tn: $tn"
echo "addrFile: $addrFile"
echo "skeyFile: $skeyFile"

# protocol parameters
ppFile=testnet/protocol-parameters.json
./cardano-cli query protocol-parameters --testnet-magic 1 --out-file $ppFile

policyFile=testnet/token.plutus
cabal exec token-policy $policyFile $oref $amt $tn



unsignedFile=testnet/tx.unsigned
signedFile=testnet/tx.signed
pid=$(./cardano-cli transaction policyid --script-file $policyFile)
tnHex=$(cabal exec token-name $tn)
addr=$(cat $addrFile)
# v is the value that i want to mint
v="$amt $pid.$tnHex"


# show some values

echo "Currency Symbol: $pid"
echo "Token name (hex): $tnHex"
echo "minted Value: $v"
echo "Address: $addr"


      
./cardano-cli transaction build \
    --testnet-magic 1 \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $amt $pid.$tnHex" \
    --mint "$amt $pid.$tnHex" \
    --mint-script-file $policyFile \
    --mint-redeemer-file testnet/unit.json \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    --testnet-magic 1 \
    --out-file $signedFile

cardano-cli transaction submit \
    --testnet-magic 1 \
    --tx-file $signedFile
      
      
      
      
      
      
      
