# Lobster Challenge

![Charles and his lobster](lobster.jpg "Charles and his lobster")

Charles has a lobster, but the poor creature does not yet have a name.
Let's use a simple Plutus Smart Contract on Cardano to help Charles find a name for his lobster!

We start with a [list of names](names.md) and a "secret random" number, which we will only reveal in the end.
Then we need the Community's help! We need 500 Community members who are willing to help name the lobster by
creating transactions which will add their own "random" number (from 1 to 100) to the current total.

In the end, we will reveal our own "secret random" number, add it to the total provided by the Community,
and use the result (after taking the remainder after division by the number of available names) as an index
into the list of names to pick the lobster name.

## Native Tokens

We use three distinct native tokens to help us name the lobster:

| Policy                                     | Policy Id                                                  | Token Name       | Purpose                             |
| ------------------------------------------ | ---------------------------------------------------------- | ---------------- | ----------------------------------- |
| [script](scripts/nft-mint-policy.plutus)   | `cc7888851f0f5aa64c136e0c8fb251e9702f3f6c9efcf3a60a54f419` | `LobsterNFT`     | Identifies the relevant UTxO.       |
| [script](scripts/other-mint-policy.plutus) | `fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50` | `LobsterCounter` | Stores the current "random" number. |
| [script](scripts/other-mint-policy.plutus) | `fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50` | `LobsterVotes`   | Counts the number of votes.         |

The first one, `LobsterNFT`, is an NFT and used to identified the "correct" UTxO "sitting" at the contract address.
(Recall that _anybody_ can send _anything_ _anywhere_ at _anytime_, so we can't prevent people from creating other UTxO's at the script address.
The UTxO we care about is the one holding the NFT.)

The second one, `LobsterCounter`, stores the current "random" number. It starts at zero when the contract is deployed,
and each "vote" can add a number of tokens between 1 and 100.
Once 500 people have "voted", we add our own secret number, which we picked before deploying the contract and which is a parameter to the contract and hence encoded in the script address. Finally we take the rest of dividing the sum by the number of names in the list of possible names.
This will be the final amount of `LobsterCounter` tokens "sitting" at the script address, and it will indicate the index of the chosen lobster name.

The third token, `LobsterVotes`, is used to count the number of "votes".
Each time a user interacts with the contract and increases the amount of `LobsterCounter` tokens by a "random" number,
The amount of `LobsterVotes` tokens at the script UTxO will go up by one.
Once 500 people have "voted", voting stops, and we will set the final result with a last transaction,
which will bring the amount of `LobsterVotes` tokens to its ultimate value of 501.

## Script

The validator for the naming contract is paramterized by six parameters:

| Parameter  | Value                              | Explanation                     ]
| ---------  | ---------------------------------- | ------------------------------- |
| Seed       | TO BE REVEALED                     | Our own "random" number.        |
| NFT        | `LobsterNFT`token (see above)      | Token identifying the UTxO.     |
| Counter    | `LobsterCounter` token (see above) | Token tracking "random" number. |
| Votes      | `LobsterVotes` token (see above)   | Token counting votes.           |
| Name Count | 1219                               | Number of available names.      |
| Vote Count | 500                                | Number of votes.                |

We will reveal the "seed", our own "random" contribution, in the end,
when we finalize the contract.

Using these parameter values, we get a [script](scripts/lobster.plutus) with script address `addr1w8433zk2shufk42hn4x7zznjjuqwwyfmxffcjszw5l2ulesdt3jff`.

## Example

Let's look at an example and assume our own "secret random" number is 42.

Voting might progress as follows:

| `LobsterCounter` | `Lobster Votes` | Comment |
| ----------------:| ---------------:| ------- |
|                0 |               0 | In the beginning, there have been no votes, and the counter starts at zero.                                     |
|               13 |               1 | The first Community member votes and adds 13 to the counter.                                                    |
|              113 |               2 | The second Community member adds 100.                                                                           |
|              114 |               3 | The third Community member adds 1.                                                                              |
|              ... |             ... | More Community members add their own numbers.                                                                   |
|            24782 |             500 | The last vote has been cast, which brought the counter to 24782.                                                |
|              444 |             501 | We add our own secret, 42, and get 24824. Dividing this by 1219, the number of names, gives a remainder of 444. |

In this example, the lobster will get the name with index 444, which happens to be __Stewart__.

## Voting

We will kick off the process by minting the `LobsterNFT` NFT and creating a UTxO at the script address,
which contains the NFT, but neither `LobsterCounter` nor `LobsterVotes` tokens yet,
because the counter starts at zero, and there have been no votes.

To vote, you have to create a transaction that spends the UTxO at the script address holding the NFT
and creates a new UTxO at the same address, which holds the same NFT,
one more `LobsterVotes` tokens than before and
a number between 1 and 100 more `LobsterCounter` tokens than before.

This means that this transaction will have to mint `LobsterVotes` and `LobsterCounter` tokens,
but that's fine: The minting policy of those tokens allows arbitrary minting.

The Plutus script defining the script address will make sure that all constraints are satisfied:

 - The NFT has to be present in both the spent UTxO and the newly created one.
 - The amount of `LobsterVotes` tokens must be smaller than 500 in the beginning, and it must increase by one.
 - The amount of `LobsterCounter` tokens must go up by a number between 1 and 100.

You can use this [Bash script](scripts/lobster-contribute.sh) to create, sign and submit a suitable transaction.
This assumes that you have a node running, the command line interface `cardano-cli` available and that your node socket is called `node.socket`.
The script expects seven arguments:

 - A UTxO with some funds to provide collateral and pay for transaction fees.
 - The current script UTxO.
 - The filename of a file containing your address - this is where the change will go.
 - The filename of the file containing your signing key.
 - The current amount of `LobsterCounter` tokens.
 - The new amount of `LobsterCounter` tokens (which must be 1-100 higher than the current amount).
 - The current amount of `LobsterVotes` tokens (which should be less than 500).

I also recorded an [Explainer Video](https://youtu.be/6xEAnMaov3E).

## Finalization

Once 500 votes have been cast, we will submit a final transaction to get the ultimate result and record it as the final amount of `LobsterCounter` tokens.
As stated above, we will add our own secret random number to the total and take the remainder after division by the number of names in our list.
We can't "cheat" at this step, because the Plutus contract has been paramterized by our number, and the contract will enforce that we calculate the final value correctly.

## Code

You can look at the Plutus code [here](src/Cardano/PlutusLobster/LobsterScript.hs),
where you will find the minting policies for the used native tokens and the validator enforcing the contract logic.

You can build the code with `cabal build`.

## Result

The 500 votes are in! The final counter after 500 votes was 22933. Adding our own "random" number, which was 582757474857012117487873, gives 582757474857012117510806.
Taking the remainder after division by 1219, the number of potential names, gives __522__.
Looking into the [list of names](names.md), we see that this index points to the name __Logan__.

Charles' lobster finally has his name!
