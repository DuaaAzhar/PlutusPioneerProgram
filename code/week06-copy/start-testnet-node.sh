#!/bin/bash

./cardano-node run \
   --topology ~/Documents/EritheiaProjects/cardano/plutus-pioneer-program/code/week06-copy/testnet/topology.json \
   --database-path ~/Documents/EritheiaProjects/cardano/plutus-pioneer-program/code/week06-copy/db \
   --socket-path ~/Documents/EritheiaProjects/cardano/plutus-pioneer-program/code/week06-copy/node.socket \
   --host-addr 0.0.0.0 \
   --port 1337 \
   --config ~/Documents/EritheiaProjects/cardano/plutus-pioneer-program/code/week06-copy/testnet/config.json



