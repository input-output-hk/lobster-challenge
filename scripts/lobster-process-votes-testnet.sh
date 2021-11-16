#!/bin/bash

# arguments:
#   utxo (main wallet)
#   wallet address file
#   wallet signing key file
#   old counter
#   old vote count
#   uuid voters address
#   start index for voter address
#   number of votes to process simultaneously

magic=1097911063

bodyFile=lobster-tx-body.03
outFile=lobster-tx.03
nftPolicyFile="nft-mint-policy.plutus"
nftPolicyId=`cardano-cli transaction policyid --script-file $nftPolicyFile`
otherPolicyFile="lobster-other-policy-v4.plutus"
otherPolicyId=`cardano-cli transaction policyid --script-file $otherPolicyFile`
nftAsset="$nftPolicyId.LobsterNFT"
nftValue="1 $nftAsset"
counterAsset="$otherPolicyId.LobsterCounter"
oldCounter="$4 $counterAsset"
walletAddr=$(cat $2)
lobsterScript="lobster-v4.plutus"
scriptAddr=`cardano-cli address build --payment-script-file $lobsterScript --testnet-magic $magic`
uuidVoterAddr="$6"
voterStartIndex="$7"
nbVotes="$8"
voterEndIndex=$[ $nbVotes + $voterStartIndex - 1 ]
voteAsset="$otherPolicyId.LobsterVotes"
oldVote="$5 $voteAsset"
newVote="$(($5+$nbVotes)) $voteAsset"
dqt='"'
mintRedeemer="{${dqt}constructor${dqt}:1,${dqt}fields${dqt}:[]}"


echo -e "\nWriting Mint redeemer 'MintVotes' json file"
echo "$mintRedeemer" > mintVotes.json

incCounter=0
txInVoters=""
voterSigner=""
voterSigningFile=""
echo -e "\nGenerating tx-in for voter requests"

# generate tx-in voter requests with LobsterCounter
for ((i=$voterStartIndex; i<=$voterEndIndex; i++))
do
 utxoAt=`cardano-cli query utxo --address $(cat voters/payment-$uuidVoterAddr.$i.addr) \
                     --testnet-magic $magic | grep $counterAsset | sed "s/ /_/g"`
 utxo=`echo $utxoAt | sed "s/________[^ ]*$//g" | sed -E "s/[_]+/#/g"`
 fee=`echo $utxoAt | grep -o "________[^ ]*lovelace" | sed "s/________//g" | sed "s/_/ /g"`
 counter=`echo $utxoAt | grep -o "\+_[^ ]*$counterAsset" | sed "s/+_//g" | sed "s/_$counterAsset//g"`
 incCounter=$[ $incCounter + $counter ]
 echo -e "Fetched utxo for voters/payment-$uuidVoterAddr.$i.addr\n -Utxo: $utxo \n -Fee: $fee \n -Counter: $counter"
 txInVoters=$txInVoters" --tx-in $utxo"
 voterSigner=$voterSigner" --required-signer voters/payment-$uuidVoterAddr.$i.skey"
 voterSigningFile=$voterSigningFile" --signing-key-file voters/payment-$uuidVoterAddr.$i.skey"
done


# generate tx-in for lobster with Nft
utxoAt=`cardano-cli query utxo --address $scriptAddr \
                    --testnet-magic $magic | grep $nftAsset | sed "s/ /_/g"`

utxoScript=`echo $utxoAt | sed "s/________[^ ]*$//g" | sed "s/_____/#/g"`
echo -e "Fetched utxo for $scriptAddr: $utxoScript"

newCounter="$(($4+$incCounter)) $counterAsset"

minUtxoScript=`cardano-cli transaction calculate-min-required-utxo \
                     --alonzo-era \
                     --protocol-params-file testnet-pparams.json \
                     --tx-out "$scriptAddr + $nftValue + $newCounter + $newVote" \
                     --tx-out-datum-hash 03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314 | sed "s/Lovelace //g"`

scriptAda="$minUtxoScript lovelace"
mintValue="$nbVotes $voteAsset"
echo -e "\nParameters:\n"
echo "wallet utxo: $1"
echo "script utxo: $utxoScript"
echo "nftPolicyfile: $nftPolicyFile"
echo "nftPolicyid: $nftPolicyId"
echo "otherPolicyfile: $otherPolicyFile"
echo "otherPolicyid: $otherPolicyId"
echo "nftValue: $nftValue"
echo "walletAddress: $walletAddr"
echo "LobsterScript: $lobsterScript"
echo "Lobster script address: $scriptAddr"
echo "signing key file: $3"
echo "old counter: $oldCounter"
echo "new counter: $newCounter"
echo "old vote: $oldVote"
echo "new vote: $newVote"
echo "Minting vote: $mintValue"
echo "uuidVoterAddress: $uuidVoterAddr"
echo "voterStartIndex: $voterStartIndex"
echo "Number of votes to be processed: $nbVotes"
echo "voterEndIndex: $voterEndIndex"
echo "Redeemer Value: $mintRedeemer"
echo "Minimum required UTxO for $scriptAddr: $minUtxoScript"

echo -e "\nQuerying protocol parameters"
cardano-cli query protocol-parameters --testnet-magic $magic > testnet-pparams.json


echo -e "\nBuilding process transaction"
cardano-cli transaction build \
            --alonzo-era \
            --testnet-magic $magic \
            --tx-in $1 \
            $txInVoters \
            --tx-in $utxoScript \
            --tx-in-script-file $lobsterScript \
            --tx-in-datum-value 0 \
            --tx-in-redeemer-value 0 \
            --tx-in-collateral $1 \
            $voterSigner \
            --tx-out "$scriptAddr + $scriptAda + $nftValue + $newCounter + $newVote" \
            --tx-out-datum-hash 03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314 \
            --mint "$mintValue" \
            --mint-script-file $otherPolicyFile \
            --mint-redeemer-file mintVotes.json \
            --change-address $walletAddr \
            --protocol-params-file testnet-pparams.json \
            --out-file $bodyFile

retVal=$?
if [ $retVal -ne 0 ]; then
    exit $retVal
fi

echo -e "Signing transaction"

cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $3 \
    $voterSigningFile \
    --testnet-magic $magic \
    --out-file $outFile

retVal=$?
if [ $retVal -ne 0 ]; then
    exit $retVal
fi

cardano-cli transaction submit \
            --testnet-magic $magic \
            --tx-file $outFile

retVal=$?
if [ $retVal -ne 0 ]; then
    exit $retVal
fi

echo -e "\nTransaction submitted\n"

