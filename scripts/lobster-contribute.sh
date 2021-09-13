#!/bin/bash

# arguments:
#   utxo (wallet)
#   utxo (lobster)
#   wallet address file
#   signinig key file
#   old counter
#   new counter
#   old votes

export CARDANO_NODE_SOCKET_PATH=node.socket

bodyFile=lobster-tx-body.03
outFile=lobster-tx.03
nftPolicyFile="nft-mint-policy.plutus"
nftPolicyId=$(./policyid.sh $nftPolicyFile)
otherPolicyFile="other-mint-policy.plutus"
otherPolicyId=$(./policyid.sh $otherPolicyFile)
nftValue="1 $nftPolicyId.LobsterNFT"
counterValue="$6 $otherPolicyId.LobsterCounter"
newVotes=$(($7+1))
votesValue="$newVotes $otherPolicyId.LobsterVotes"
increaseValue="$(($6-$5)) $otherPolicyId.LobsterCounter + 1 $otherPolicyId.LobsterVotes"
walletAddr=$(cat $3)
scriptFile=lobster.plutus
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
echo "votesValue: $votesValue"
echo "walletAddress: $walletAddr"
echo "scriptFile: $scriptFile"
echo "scriptAddress: $scriptAddr"
echo "signing key file: $4"
echo "old counter: $5"
echo "new counter: $6"
echo "increaseValue: $increaseValue"
echo "old votes: $7"
echo "new votes: $newVotes"
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
    --tx-in-datum-value [] \
    --tx-in-redeemer-value [] \
    --tx-in-collateral $1 \
    --tx-out "$scriptAddr + 2034438 lovelace + $nftValue + $counterValue + $votesValue" \
    --tx-out-datum-hash 45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0 \
    --mint "$increaseValue" \
    --mint-script-file $otherPolicyFile \
    --mint-redeemer-value [] \
    --change-address $walletAddr \
    --protocol-params-file mainnet-protocol-parameters.json \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

./cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $4 \
    --mainnet \
    --out-file $outFile

echo "signed transaction and saved as $outFile"

./cardano-cli transaction submit \
    --mainnet \
    --tx-file $outFile

echo "submitted transaction"

echo
