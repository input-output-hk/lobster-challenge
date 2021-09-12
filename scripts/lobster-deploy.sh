#!/bin/bash

# arguments:
#   utxo (NFT)
#   utxo (collateral)
#   wallet address file
#   signing key file
export CARDANO_NODE_SOCKET_PATH=node.socket

bodyFile=lobster-tx-body.02
outFile=lobster-tx.02
nftPolicyFile="nft-mint-policy.plutus"
nftPolicyId=$(./policyid.sh $nftPolicyFile)
value="1724100 lovelace + 1 $nftPolicyId.LobsterNFT"
walletAddr=$(cat $3)
scriptAddr=$(./mainnet-script-address.sh lobster.plutus)

echo "utxoNFT: $1"
echo "utxoCollateral: $2"
echo "bodyFile: $bodyFile"
echo "outFile: $outFile"
echo "nftPolicyFile: $nftPolicyFile"
echo "nftPolicyId: $nftPolicyId"
echo "value: $value"
echo "walletAddress: $walletAddr"
echo "scriptAddress: $scriptAddr"
echo "signing key file: $4"
echo

echo "querying protocol parameters"
./mainnet-query-protocol-parameters.sh

echo

./cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in $1 \
    --tx-in $2 \
    --tx-in-collateral $2 \
    --tx-out "$scriptAddr + $value" \
    --tx-out-datum-hash 45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0 \
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
