#!/bin/bash

# arguments:
#   utxo (NFT)
#   utxo (collateral)
#   wallet address file
#   signing key file

magic=1097911063

bodyFile=lobster-tx-body.02
outFile=lobster-tx.02
nftPolicyFile="nft-mint-policy.plutus"
nftPolicyId=`cardano-cli transaction policyid --script-file $nftPolicyFile`
value="1724100 lovelace + 1 $nftPolicyId.LobsterNFT"
walletAddr=$(cat $3)
lobsterScript="lobster-v4.plutus"
scriptAddr=`cardano-cli address build --payment-script-file $lobsterScript --testnet-magic $magic`

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

echo -e "\nQuerying protocol parameters"

cardano-cli query protocol-parameters --testnet-magic $magic > testnet-pparams.json

echo -e "\nBuilding lobster transaction"

cardano-cli transaction build \
            --alonzo-era \
            --testnet-magic $magic \
            --tx-in $1 \
            --tx-in $2 \
            --tx-in-collateral $2 \
            --tx-out "$scriptAddr + $value" \
            --tx-out-datum-hash 03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314 \
            --change-address $walletAddr \
            --protocol-params-file testnet-pparams.json \
            --out-file $bodyFile

echo -e "\nTransaction saved to $bodyFile"

cardano-cli transaction sign \
            --tx-body-file $bodyFile \
            --signing-key-file $4 \
            --testnet-magic $magic \
            --out-file $outFile

echo -e "\nSigned transaction saved to $outFile"

cardano-cli transaction submit \
            --testnet-magic $magic \
            --tx-file $outFile

echo -e "\nTransaction submitted\n"
