#!/bin/bash

export CARDANO_NODE_SOCKET_PATH=node.socket
./cardano-cli query utxo \
    --mainnet \
    --address $(cat $1)
