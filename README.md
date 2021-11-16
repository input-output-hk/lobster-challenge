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

This repository corresponds to a first viable solution addressing concurrency using a batching pattern.
Here, instead of locking voting requests in an intermediate **_request_** script, users directly submit their votes to their own public key address.
The intermediate **_request_** script is only used to notify the presence of pending votes and to lock transaction fees that can afterwards be claimed by the _batcher_. When aggregating votes in one single transaction, the batcher only inspects the UTXOS sitting at the **_request_** script to collect the respective tokens at the voters's public key addresses. This avoids triggering the execution of the **_request_** script for each pending votes.
However, users are required to sign the aggregated transaction to authorize the spending of voting orders. They should therefore be online to participate.

## Repository Organisation
In the repository, policy scripts are specified in file LobsterPolicies.hs while validator scripts are specified in LobsterScript.hs
The following executables are also produced when building the project:
 - **plutus-lobster-tokens**: to generate the **_Lobster NFT policy_** script 
 - **plutus-lobster**: to generate:
      - The **_lobster_** script: main script containing the voting logic
      - The **_request_** script: script use to notify pending votes and to collect fees
      - The **_end_** script: script for burning ticket tokens once the batcher has collected fees for processed votes
      - The **_minting _counter and finished tokens_** policy script: for minting the counter tokens representing votes and the finished token once votes are tallied
      - The **_ticket_** policy script: for minting ticket tokens required by users for submitting votes

## Overall Architecture
The overall architecture for this new version of the lobster contract is as follows:

### Deploying the Lobster contract
 - An NFT token is first minted using the **_nftPolicy_** script. This NFT is used to identify the "correct" UTxO "sitting" at the lobster contract address and to guarantee the uniquness of the script addresses.
 - The batcher public key address is also used to parameterize the lobster contract. He is the only one authorize to consume the UTXO sitting at the contract address.
 - The lobster contract is deployed without any counter tokens.

### Vote Submission
  - The **_OtherPolicy_** minting script is used to mint counter tokens (representing votes) to be submitted to the user's public key address. Note that counter tokens are minted when redeemer `MintCounters` is passed on as argument to the script. Script **_OtherPolicy_** checks that the proper counter token amount is minted, that the voting deadline is still valid and that the fees are sufficient. It also checks that the minted tokens are properly submitted to the voter's public key address and that a proper UTXO is created at the **__request_** script address (see below).
  - The **_ticketPolicy_** minting script is used to mint a single ticket token to be used when submitting votes. Note that the token name is set to the public key hash of the submitter. This is enforced by the script when the ticket token is minted.
  - For the lobster contract, the batcher fees are split into two categories and are referred respectively as **_submit fees_** and **_collect fees_**. The _submit fees_ are directly sent to the users's public key address together with the counter and ticket tokens. As for the _collect fees_ they are locked at the **_request_** script as described below.
  - The notification of a pending vote is done by submitting the voter's public key hash and the **_collect fees_** to the **_request_** script.

### Processing Votes
  - The batcher inspects the UTXOs sitting at the **_request_** script address to collect the counter and ticket tokens at the voters' public key addresses and to build the aggregated transaction. Note that if no counter token (respectively ticket token) is present at the corresponding public key address or the **_submit fees_** are insufficient, the vote request is considered as void.
  - The UTXOS sitting at the **_request_** script are not consumed by the aggregated transaction. Only the UTXOS at the voters' public key addresses are consumed. At this stage, the batcher can also collect the **_submit fees_** but not the **_collect fees_**.
  - Voters are also required to sign the aggregated transaction before its submission.
  - When the aggregated transaction is submitted, the **_lobster_** script checks that:
      - The NFT lobster token sits at the script output;
      - The batcher has signed the aggregated transaction;
      - Each consumed UTXO having counter tokens also has an associated ticket token containing the public key hash of the voter (i.e., pub key hash referenced by the UTXO);
      - The number of counter tokens sitting at the script output includes the tailled votes; and
      - Voting deadline is still valid
   - The ticket tokens for the processed votes are collected by the batcher for claiming the additional fees sitting at the **_request_** script.

### Claiming Fees
  - Once the batcher has collected the ticket tokens when processing the votes via the **_lobster_** script, he can claim the corresponding **_collect fees_** locked at the **_request_** script. When submitting the _claim_ transaction, the following checks are performed by the **_request_** script:
     - The transaction is properly signed by the batcher.
     - There does not exist two or more UTXOs at the script input for the same public key hash (i.e., residing as Datum). This check is essential as it ensures that the batcher cannot provide only one ticket token to claim fees for more than one vote submitted by the same user and within a single transaction. Hence, if a user has submitted several votes, the batcher can only claim the respective fees via seperate transaction. Nevertheless, claiming fees for more than one vote within a single transaction is possible only when the aforementioned condition is satisfied.
     - A ticket token is present as input for each consuming UTXO.
     - The ticket tokens are sent to the **_end_** script to be locked forever (e.g., burning).
     
### Finializing Votes
TBD
