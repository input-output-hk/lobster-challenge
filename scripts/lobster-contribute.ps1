#Requires -Version 7.0

<#
.SYNOPSIS
    Analog of lobster-contribute.sh in Powershell 
.PARAMETER WalletUtxo
    This directly corresponds to the first argument of lobster-contribute.sh.
.PARAMETER WalletAddress
    Wallet address of the form "addr..." - this corresponds to the content of
    the third argument "wallet address file" of lobster-contribute.sh
.PARAMETER SigningKeyFile
    This directly corresponds to the forth argument of lobster-contribute.sh.
.PARAMETER Vote
    Value in [1,100] that corresponds to the difference of the fifth and sixth
    argument of lobster-contribute.sh.
#>

[CmdletBinding()]
 param (
     [Parameter(Mandatory=$true)][string] $WalletUtxo,
     [Parameter(Mandatory=$true)][string] $WalletAddress,
     [Parameter(Mandatory=$true)][string] $SigningKeyFile,
     [Parameter(Mandatory=$true)][ValidateRange(1,100)][int] $Vote
 )

Set-StrictMode -Version 3.0
$ErrorActionPreference = "Stop"

function Find-CardanoNode {
    try {
        $CardanoNode = Get-Process cardano-node -ErrorAction Stop
    } catch [Microsoft.PowerShell.Commands.ProcessCommandException]  {
        "Could not connect to local cardano node! Is Daedalus running?" | Write-Error
        exit 1
    }
    $Signature = $CardanoNode.Path | Get-AuthenticodeSignature
    if ($Signature.Status -ne "Valid") {
        throw "The running cardano node not signed!"
    }
    if ($Signature.SignerCertificate.Thumbprint -ne "DCFD5AE14FCF6C13F7F7FC59D005DD7CFEDFA212") {
        throw "The running cardano node not signed by IOHK!"
    }
    return $CardanoNode
}

function Set-CardanoEnvironment {
    $CardanoCli = Get-Command "cardano-cli.exe" -CommandType Application -ErrorAction SilentlyContinue
    if (($null -ne $CardanoCli) -and (Test-Path Env:CARDANO_NODE_SOCKET_PATH)) {
        "Cardano environment already set at {0} and socket path {1}" -f $CardanoCli.Source, $Env:CARDANO_NODE_SOCKET_PATH | Write-Host
        return
    }
    $CardanoNode = Find-CardanoNode
    $CardanoPath = $CardanoNode | Select-Object -ExpandProperty Path | Split-Path -Parent
    "Found cardano-node.exe in $CardanoPath" | Write-Host
    $Env:PATH = (@($Env:PATH -split ";") + $CardanoPath) -join ";"

    $CardanoCli = $CardanoPath | Join-Path -ChildPath "cardano-cli.exe" | Get-Item -ErrorAction Stop

    $ClList = ($CardanoNode | Select-Object -ExpandProperty CommandLine) -split " "
    $SocketPath = $ClList[[array]::indexof($ClList,"--socket-path")+1]
    "Found socket path $SocketPath" | Write-Host
    $Env:CARDANO_NODE_SOCKET_PATH = $SocketPath
}

Set-CardanoEnvironment


$NftPolicyFile = "nft-mint-policy.plutus"
$NftPolicyId = & cardano-cli transaction policyid --script-file $NftPolicyFile
$OtherPolicyFile = "other-mint-policy.plutus"
$OtherPolicyId = & cardano-cli transaction policyid --script-file $OtherPolicyFile
$ScriptFile = "lobster.plutus"
$ScriptAddress = & cardano-cli address build --payment-script-file $ScriptFile --mainnet

$TempFile = New-TemporaryFile
& cardano-cli query utxo --mainnet --address $ScriptAddress --out-file $TempFile.FullName
$UtxosAtScriptAddress = $TempFile.FullName | Get-Item | Get-Content | ConvertFrom-Json -AsHashtable
$TempFile | Remove-Item
$ValidLobsterUtxos = @($UtxosAtScriptAddress.Keys | Where-Object { $UtxosAtScriptAddress[$_].value.ContainsKey($NftPolicyId) -and $UtxosAtScriptAddress[$_].value[$NftPolicyId].ContainsKey("LobsterNFT")})
if ($ValidLobsterUtxos.length -ne 1) {
    "Could not identify the correct UTXO with the Lobster NFT!" | Write-Host
    exit 1
}
$LobsterUtxo = $ValidLobsterUtxos | Select-Object -First 1
$OldVotes = [int] $UtxosAtScriptAddress[$LobsterUtxo].value[$OtherPolicyId]["LobsterVotes"]
$OldCounter = [int] $UtxosAtScriptAddress[$LobsterUtxo].value[$OtherPolicyId]["LobsterCounter"]

$NftValue = "1 {0}.LobsterNFT" -f $NftPolicyId
$NewCounter = $OldCounter + $Vote
$CounterValue = "{0} {1}.LobsterCounter" -f $NewCounter, $OtherPolicyId
$NewVotes = $OldVotes + 1
$VotesValue = "$NewVotes $otherPolicyId.LobsterVotes"
$IncreaseValue = "{0} {1}.LobsterCounter + 1 {2}.LobsterVotes" -f $Vote, $OtherPolicyId, $OtherPolicyId

$BodyFile = "lobster-tx-body.03"
$OutFile = "lobster-tx.03"

"wallet utxo: $WalletUtxo",
"script utxo: $LobsterUtxo",
"bodyfile: $BodyFile",
"outfile: $OutFile",
"nftPolicyfile: $NftPolicyFile",
"nftPolicyid: $NftPolicyId",
"otherPolicyfile: $OtherPolicyFile",
"otherPolicyid: $OtherPolicyId",
"nftValue: $NftValue",
"counterValue: $CounterValue",
"votesValue: $VotesValue",
"walletAddress: $WalletAddress",
"scriptFile: $ScriptFile",
"scriptAddress: $ScriptAddress",
"signing key file: $SigningKeyFile",
"old counter: $OldCounter",
"new counter: $NewCounter",
"increaseValue: $IncreaseValue",
"old votes: $OldVotes",
"new votes: $NewVotes" | Write-Host

$MainnetParameters = "mainnet-protocol-parameters.json"
if ($MainnetParameters | Test-Path) { $MainnetParameters | Remove-Item }
& cardano-cli query protocol-parameters --mainnet --out-file $MainnetParameters

$BuildTransactionArguments = "transaction build",
    "--alonzo-era",
    "--mainnet",
    "--tx-in $WalletUtxo",
    "--tx-in $LobsterUtxo",
    "--tx-in-script-file $scriptFile",
    "--tx-in-datum-value []",
    "--tx-in-redeemer-value []",
    "--tx-in-collateral $WalletUtxo",
    "--tx-out `"$ScriptAddress + 2034438 lovelace + $NftValue + $CounterValue + $VotesValue`"",
    "--tx-out-datum-hash 45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    "--mint `"$IncreaseValue`"",
    "--mint-script-file $OtherPolicyFile",
    "--mint-redeemer-value []",
    "--change-address $WalletAddress",
    "--protocol-params-file $MainnetParameters",
    "--out-file $BodyFile"

$BuildTransactionProcess = Start-Process -Path cardano-cli.exe -ArgumentList $BuildTransactionArguments -NoNewWindow -PassThru
$BuildTransactionProcess | Wait-Process
if ($BuildTransactionProcess.ExitCode -ne 0) {
    "Building the transaction failed!" | Write-Error
    exit $BuildTransactionProcess.ExitCode
}

"saved transaction to $BodyFile" | Write-Host

$SignTransactionArguments = "transaction sign",
    "--tx-body-file $BodyFile",
    "--signing-key-file $SigningKeyFile",
    "--mainnet",
    "--out-file $OutFile"

$SignTransactionProcess = Start-Process -Path cardano-cli.exe -ArgumentList $SignTransactionArguments -NoNewWindow -PassThru
$SignTransactionProcess | Wait-Process
if ($SignTransactionProcess.ExitCode -ne 0) {
    "Signing the transaction failed!" | Write-Error
    exit $SignTransactionProcess.ExitCode
}

"signed transaction and saved as $OutFile" | Write-Host

$Answer = Read-Host -Prompt "Submit Transaction?"

if ($Answer -eq "y") {
    $SubmitTransactionArguments = "transaction submit", "--mainnet", "--tx-file $OutFile"
    $SubmitTransactionProcess = Start-Process -Path cardano-cli.exe -ArgumentList $SubmitTransactionArguments -NoNewWindow -PassThru
    $SubmitTransactionProcess | Wait-Process
    if ($SubmitTransactionProcess.ExitCode -ne 0) {
        "Submitting the transaction failed!" | Write-Error
        exit $SubmitTransactionProcess.ExitCode
    } else {
        "submitted transaction" | Write-Host
    }
} else {
    "cancelled" | Write-Host
}
