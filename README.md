# Spooky Pumpkin Challenge

![spooky pumpkin](spooky_pumpkin.jpg)

We have a really spooky pumpkin, but the poor thing does not yet have a name.
Let's use a simple Plutus Smart Contract on Cardano to help find a name for our pumpkin!

We start with a [list of names](names.md) and a "secret random" number, which we will only reveal in the end.
Then we need the Community's help! We need Community members who are willing to help name the pumpkin by
creating transactions which will add their own "random" number (from 1 to 100) to the current total.

In the end, we will reveal our own "secret random" number, add it to the total provided by the Community,
and use the result (after taking the remainder after division by the number of available names) as an index
into the list of names to pick the pumpkin name.

This spooky pumpkin challenge is based on the "lobster challenge", which we ran to celebrate our successful Alonzo hardfork event.
For this reason, the term "lobster" still appears all over the place.

## Native Tokens

We use three distinct native tokens to help us name the pumpkin:

| Policy                                     | Policy Id                                                  | Token Name        | Purpose                                     |
| ------------------------------------------ | ---------------------------------------------------------- | ----------------- | ------------------------------------------- |
| [script](scripts/nft-mint-policy.plutus)   | `7bd8fe026f87948f978c8214a34d81dc93cbc46cee1c8ba80cdefe80` | `LobsterNFT`      | Identifies the relevant UTxO.               |
| [script](scripts/other-mint-policy.plutus) | `fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50` | `LobsterCounter`  | Stores the current "random" number.         |
| [script](scripts/other-mint-policy.plutus) | `fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50` | `LobsterFinished` | Indicates whether the contract is finished. |

The first one, `LobsterNFT`, is an NFT and used to identified the "correct" UTxO "sitting" at the contract address.
(Recall that _anybody_ can send _anything_ _anywhere_ at _anytime_, so we can't prevent people from creating other UTxO's at the script address.
The UTxO we care about is the one holding the NFT.)

The second one, `LobsterCounter`, stores the current "random" number. It starts at zero when the contract is deployed,
and each "vote" can add a number of tokens between 1 and 100.
Once the deadline (Thursday, Ocober 28, 2021, 6PM GMT) has passed, we add our own secret number, which we picked before deploying the contract and which is a parameter to the contract and hence encoded in the script address. Finally we take the rest of dividing the sum by the number of names in the list of possible names.
This will be the final amount of `LobsterCounter` tokens "sitting" at the script address, and it will indicate the index of the chosen pumpkin name.
The fact that this last step has been performed will be indicated by the presence of one coint of the `LobsterFinished` token.

## Script

The validator for the naming contract is paramterized by six parameters:

| Parameter  | Value                               | Explanation                         |
| ---------  | ----------------------------------- | ----------------------------------- |
| Seed       | TO BE REVEALED                      | Our own "random" number.            |
| NFT        | `LobsterNFT`token (see above)       | Token identifying the UTxO.         |
| Counter    | `LobsterCounter` token (see above)  | Token tracking "random" number.     |
| Finished   | `LobsterFinished` token (see above) | Token counting votes.               |
| Name Count | 1219                                | Number of available names.          |
| Deadline   | 1635444000000                       | Deadline (October 28, 2021, 6PM GMT |

We will reveal the "seed", our own "random" contribution, in the end,
when we finalize the contract.

Using these parameter values, we get a [script](scripts/lobster.plutus) with script address `addr1w8ydgw5twuk724uh4jcmjsflkerwj9wvgeneuku577w9a4s7avmxg`.

## Example

Let's look at an example and assume our own "secret random" number is 42.

Voting might progress as follows:

| `LobsterCounter` | `Lobster Finished` | Comment                                                                                                         |
| ----------------:| ------------------:| --------------------------------------------------------------------------------------------------------------- |
|                0 |                  0 | In the beginning, there have been no votes, and the counter starts at zero.                                     |
|               13 |                  0 | The first Community member votes and adds 13 to the counter.                                                    |
|              113 |                  0 | The second Community member adds 100.                                                                           |
|              114 |                  0 | The third Community member adds 1.                                                                              |
|              ... |                ... | More Community members add their own numbers.                                                                   |
|            24782 |                  0 | The last vote has been cast, which brought the counter to 24782.                                                |
|              444 |                  1 | We add our own secret, 42, and get 24824. Dividing this by 1219, the number of names, gives a remainder of 444. |

In this example, the pumpkin will get the name with index 444, which happens to be __Stewart__.

## Voting

We will kick off the process by minting the `LobsterNFT` NFT and creating a UTxO at the script address,
which contains the NFT, but neither `LobsterCounter` nor `LobsterFinished` tokens yet,
because the counter starts at zero, and the contract is not yet finished.

To vote, you have to create a transaction that spends the UTxO at the script address holding the NFT
and creates a new UTxO at the same address, which holds the same NFT,
and a number between 1 and 100 more `LobsterCounter` tokens than before.

This means that this transaction will have to mint `LobsterCounter` tokens,
but that's fine: The minting policy of those tokens allows arbitrary minting.

The Plutus script defining the script address will make sure that all constraints are satisfied:

 - The NFT has to be present in both the spent UTxO and the newly created one.
 - The deadline must not have been reached.
 - The amount of `LobsterCounter` tokens must go up by a number between 1 and 100.

You can use this [Bash script](scripts/lobster-contribute-deadline.sh) to create, sign and submit a suitable transaction.
This assumes that you have a node running, the command line interface `cardano-cli` available and that your node socket is called `node.socket`.
The script expects six arguments:

 - A UTxO with some funds to provide collateral and pay for transaction fees.
 - The current script UTxO.
 - The filename of a file containing your address - this is where the change will go.
 - The filename of the file containing your signing key.
 - The current amount of `LobsterCounter` tokens.
 - The new amount of `LobsterCounter` tokens (which must be 1-100 higher than the current amount).

To get the current script UTxO and the current amount of `LobsterCounter` tokens, you can use [this](scripts/mainnet-utxo-at.sh) Bash script,
which expects the contract address `addr1w8ydgw5twuk724uh4jcmjsflkerwj9wvgeneuku577w9a4s7avmxg` as argument and prints all UTxO's at that address to the console.
To identify the right UTxO, look for the one containing the `LobsterNFT` token!

## Finalization

Once the deadline has been reached, we will submit a final transaction to get the ultimate result and record it as the final amount of `LobsterCounter` tokens.
As stated above, we will add our own secret random number to the total and take the remainder after division by the number of names in our list.
We can't "cheat" at this step, because the Plutus contract has been paramterized by our number, and the contract will enforce that we calculate the final value correctly.

## Code

You can look at the Plutus code [here](src/Cardano/PlutusLobster/LobsterDeadlineScript.hs) and [here](src/Cardano/PlutusLobster/LobsterPolicies.hs),
where you will find the validator enforcing the contract logic and the minting policies for the used native tokens.

You can build the code with `cabal build`.
