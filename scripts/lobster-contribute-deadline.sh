#!/bin/bash

# arguments:
#   utxo (wallet)
#   utxo (lobster)
#   wallet address file
#   signinig key file
#   old counter
#   new counter

export CARDANO_NODE_SOCKET_PATH=node.socket

bodyFile=lobster-tx-body.03
outFile=lobster-tx.03
nftPolicyFile="nft-mint-policy.plutus"
nftPolicyId=$(./policyid.sh $nftPolicyFile)
otherPolicyFile="other-mint-policy.plutus"
otherPolicyId=$(./policyid.sh $otherPolicyFile)
nftValue="1 $nftPolicyId.LobsterNFT"
counterValue="$6 $otherPolicyId.LobsterCounter"
increaseValue="$(($6-$5)) $otherPolicyId.LobsterCounter"
invalidBefore=$(./mainnet-current-slot.sh)
invalidHereafter=$(($invalidBefore+120))
walletAddr=$(cat $3)
scriptFile=lobster-deadline.plutus
scriptAddr=$(./mainnet-script-address.sh $scriptFile)

echo "wallet utxo: $1"
echo "script utxo: $2"
echo "bodyfile: $bodyFile"
echo "outfile: $outFile"
echo "nftPolicyfile: $nftPolicyFile"
echo "nftPolicyid: $nftPolicyId"
echo "otherPolicyfile: $otherPolicyFile"
echo "otherPolicyid: $otherPolicyId"
echo "nftValue: $nftValue"
echo "counterValue: $counterValue"
echo "walletAddress: $walletAddr"
echo "scriptFile: $scriptFile"
echo "scriptAddress: $scriptAddr"
echo "signing key file: $4"
echo "old counter: $5"
echo "new counter: $6"
echo "increaseValue: $increaseValue"
echo "invalidBefore: $invalidBefore"
echo "invalidHereafter: $invalidHereafter"
echo

echo "querying protocol parameters"
./mainnet-query-protocol-parameters.sh

echo

./cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in $1 \
    --tx-in $2 \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-value 0 \
    --tx-in-redeemer-value 0 \
    --tx-in-collateral $1 \
    --tx-out "$scriptAddr + 1930992 lovelace + $nftValue + $counterValue" \
    --tx-out-datum-hash 03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314 \
    --mint "$increaseValue" \
    --mint-script-file $otherPolicyFile \
    --mint-redeemer-value [] \
    --change-address $walletAddr \
    --protocol-params-file mainnet-protocol-parameters.json \
    --invalid-before $invalidBefore \
    --invalid-hereafter $invalidHereafter \
    --out-file $bodyFile

retVal=$?
if [ $retVal -ne 0 ]; then
    exit $retVal
fi

echo "saved transaction to $bodyFile"

./cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $4 \
    --mainnet \
    --out-file $outFile

retVal=$?
if [ $retVal -ne 0 ]; then
    exit $retVal
fi

echo "signed transaction and saved as $outFile"

./cardano-cli transaction submit \
    --mainnet \
    --tx-file $outFile

retVal=$?
if [ $retVal -ne 0 ]; then
    exit $retVal
fi

echo "submitted transaction"

echo
