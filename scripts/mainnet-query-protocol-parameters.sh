#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=node.socket
./cardano-cli query protocol-parameters \
    --mainnet \
    --out-file "mainnet-protocol-parameters.json"
