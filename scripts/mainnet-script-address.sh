#!/bin/bash

./cardano-cli address build \
    --payment-script-file $1 \
    --mainnet
