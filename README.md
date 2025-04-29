# launch-testnet

A minimal Haskell CLI that embeds default Cardano genesis specs and node config/topology files, generates testnet data with `cardano-cli`, and immediately launches a single‐pool local testnet.

## Prerequisites

- GHC (8.10+) and Cabal (3.0+) installed  
- `cardano-node-10.x` and `cardano-cli-10.x` binaries on your `PATH`

## Building

- Fetch dependencies and compile:

   ```bash
   cabal update
   cabal build

   ```

## Usage

All commands assume `cardano-node` and `cardano-cli` are on your `PATH`.

```bash
$ launch-testnet 

launch-testnet - spin up or dump spec files for a local Cardano testnet

Usage: launch-testnet COMMAND

  Commands: default, dump-spec-files, custom

Available options:
  -h,--help                Show this help text

Available commands
  default                  Launch testnet with default embedded specs
  dump-spec-files          Dump embedded specs + config/topology into DIR
  custom                   Use your own genesis specs & node config + topology
```  

### 1. default  
Launch a Conway‐era single‐pool testnet in Conway era, with `0.100 s` slots and `5 min` epochs; all other protocol Parameters match current Mainnet protocol paramters. 

```shell
$ cabal run launch-testnet -- default --out-dir <DIR>
```
- Writes out under <DIR>/:
    specs/{shelley.json, alonzo.json, conway.json}
    configuration.json
    topology.json
- Runs:
```
  cardano-cli conway genesis create-testnet-data \
    --spec-shelley <DIR>/specs/shelley.json \
    --spec-alonzo  <DIR>/specs/alonzo.json \
    --spec-conway  <DIR>/specs/conway.json \
    --pools 1 --stake-delegators 3 \
    --total-supply 43000000000000 \
    --delegated-supply 9000000000000 \
    --testnet-magic 42 \
    --out-dir <DIR>
```
- Then launches:
```
  cardano-node run \
    --config      <DIR>/configuration.json \
    --topology    <DIR>/topology.json \
    --database-path <DIR>/db \
    --socket-path   <DIR>/node.sock \
    --shelley-kes-key     <DIR>/pools-keys/pool1/kes.skey \
    --shelley-vrf-key     <DIR>/pools-keys/pool1/vrf.skey \
    --byron-delegation-certificate <DIR>/pools-keys/pool1/byron-delegation.cert \
    --byron-signing-key            <DIR>/pools-keys/pool1/byron-delegate.key \
    --shelley-operational-certificate <DIR>/pools-keys/pool1/opcert.cert \
    --host-addr 0.0.0.0 \
    --port      6000 \
    | tee -a <DIR>/pools-keys/pool1/node.log
```

### 2. dump-spec-files  
Only write the default spec files, config & topology into <DIR>:

```shell
$ cabal run launch-testnet -- dump-spec-files --out-dir <DIR>
```
### 3. custom  
Provide your own genesis spec files plus node config & topology:
```shell
$ cabal run launch-testnet -- custom \
    --shelley-spec <PATH>/shelley.json \
    --alonzo-spec  <PATH>/alonzo.json \
    --conway-spec  <PATH>/conway.json \
    --config       <PATH>/configuration.json \
    --topology     <PATH>/topology.json \
    --out-dir      <DIR>
```
- Validates each input file exists.  
- Copies them into <DIR>/specs/…, <DIR>/configuration.json and <DIR>/topology.json.  
- Runs the same cardano-cli conway genesis create-testnet-data … --out-dir <DIR>  
- Launches cardano-node run against your provided config + topology, logging to <DIR>/node.log.

