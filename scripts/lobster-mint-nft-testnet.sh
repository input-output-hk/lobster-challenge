#!/bin/bash

# arguments:
#   utxo
#   wallet address file
#   signing key file

magic=1097911063

bodyFile=lobster-tx-body.01
outFile=lobster-tx.01
nftPolicyFile="nft-mint-policy.plutus"
nftPolicyId=`cardano-cli transaction policyid --script-file $nftPolicyFile`
value="1 $nftPolicyId.LobsterNFT"
walletAddr=$(cat $2)

echo "utxo: $1"
echo "bodyFile: $bodyFile"
echo "outFile: $outFile"
echo "nftPolicyFile: $nftPolicyFile"
echo "nftPolicyId: $nftPolicyId"
echo "value: $value"
echo "walletAddress: $walletAddr"
echo "signing key file: $3"
echo

echo -e "\nQuerying protocol parameters"

cardano-cli query protocol-parameters --testnet-magic $magic > testnet-pparams.json

echo -e "\nBuilding nft minting transaction"

cardano-cli transaction build \
            --alonzo-era \
            --testnet-magic $magic \
            --tx-in $1 \
            --tx-in-collateral $1 \
            --tx-out "$walletAddr + 1724100 lovelace + $value" \
            --mint "$value" \
            --mint-script-file $nftPolicyFile \
            --mint-redeemer-value [] \
            --change-address $walletAddr \
            --protocol-params-file testnet-pparams.json \
            --out-file $bodyFile

echo -e "\nTransaction saved to $bodyFile"


echo -e "\nSigning transaction"
cardano-cli transaction sign \
            --tx-body-file $bodyFile \
            --signing-key-file $3 \
            --testnet-magic $magic \
            --out-file $outFile

echo -e "\nSigned transaction saved to $outFile"

cardano-cli transaction submit \
            --testnet-magic $magic \
            --tx-file $outFile

echo -e "Transaction submitted\n"
