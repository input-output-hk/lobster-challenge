#!/bin/bash

# arguments:
#  utxo
#  voter address file
#  voter verification key file (vkey)
#  voter signing key file (skey)
#  fee
#  vote

magic=1097911063
walletAddr=$(cat $2)
voteTx=voter-tx.raw
signTx=voter-tx.sign
otherPolicyFile="lobster-other-policy-v4.plutus"
otherPolicyId=`cardano-cli transaction policyid --script-file $otherPolicyFile`
feeValue="$5 lovelace"
counterValue="$6 $otherPolicyId.LobsterCounter"
requestScript="lobster-request-v4.plutus"
scriptAddr=`cardano-cli address build --payment-script-file $requestScript --testnet-magic $magic`
pubKeyHash=`cardano-cli address key-hash --payment-verification-key-file $3`
dqt='"'
mintRedeemer="{${dqt}constructor${dqt}:0,${dqt}fields${dqt}:[]}"
pkhDatum="{${dqt}bytes${dqt}:${dqt}$pubKeyHash${dqt}}"


echo -e "\nWriting Mint redeemer 'MintCounters' json file"
echo "$mintRedeemer" > mintCounters.json

echo -e "\nWriting PubKeyHash Datum Json file\n"
echo "$pkhDatum" > pkh.json


minUtxoWallet=`cardano-cli transaction calculate-min-required-utxo \
                     --alonzo-era \
                     --protocol-params-file testnet-pparams.json \
                     --tx-out "$walletAddr + $feeValue + $counterValue" | sed "s/Lovelace //g"`

# setting min fee
if [ "$5" -lt "$minUtxoWallet" ]
then
 echo -e "\nAdjusting Fee Value\n"
 feeValue="$minUtxoWallet lovelace"
fi

minUtxoScript=`cardano-cli transaction calculate-min-required-utxo \
                     --alonzo-era \
                     --protocol-params-file testnet-pparams.json \
                     --tx-out "$scriptAddr + 1 lovelace" \
                     --tx-out-datum-embed-file pkh.json | sed "s/Lovelace //g"`

scriptAda="$minUtxoScript lovelace"

echo -e "\nParameters:\n"
echo "utxo: $1"
echo "voterAddr: $walletAddr"
echo "Public Key Hash: $pubKeyHash"
echo "otherPolicyFile: $otherPolicyFile"
echo "otherPolicyId: $otherPolicyId"
echo "Original Fee Value: $5 lovelace"
echo "Adjusted Fee Value $feeValue"
echo "counter Value: $counterValue"
echo "verification key file: $3"
echo "signing key file: $4"
echo "Request script address: $scriptAddr"
echo "Redeemer Value: $mintRedeemer"
echo "Datum Value: $pkhDatum"
echo "Minimum required UTxO for $walletAddr: $minUtxoWallet"
echo "Minimum required UTxO for $scriptAddr: $minUtxoScript"

echo -e "\nQuerying protocol parameters"
cardano-cli query protocol-parameters --testnet-magic $magic > testnet-pparams.json

echo -e "\nWriting Mint redeemer 'MintCounters' json file"
echo "$mintRedeemer" > mintCounters.json

echo -e "\nBuilding voting transaction"
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic $magic \
    --tx-in $1 \
    --tx-in-collateral $1 \
    --required-signer $4 \
    --tx-out "$walletAddr + $feeValue + $counterValue" \
    --tx-out "$scriptAddr + $scriptAda" \
    --tx-out-datum-embed-file pkh.json \
    --mint "$counterValue" \
    --mint-script-file $otherPolicyFile \
    --mint-redeemer-file mintCounters.json \
    --change-address $walletAddr \
    --protocol-params-file testnet-pparams.json \
    --out-file $voteTx

echo -e "\nSigning transaction"
cardano-cli transaction sign \
            --tx-body-file $voteTx \
            --signing-key-file $4 \
            --testnet-magic $magic \
            --out-file $signTx

cardano-cli transaction submit \
            --testnet-magic $magic \
            --tx-file $signTx

echo -e "\nTransaction submitted\n"

