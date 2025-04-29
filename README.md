# launch-testnet

A minimal Haskell CLI tool to quickly set up a local Cardano testnet environment. It embeds default Cardano genesis specs and node configuration files, generates testnet data using `cardano-cli`, and launches a single-pool node with `cardano-node`. Ideal for developers needing a quick local Conway-era testnet.

## Prerequisites

* **GHC:** Version 8.10.7 or later
* **Cabal:** Version 3.0 or later
* **Cardano Binaries:** `cardano-node` and `cardano-cli` executables must be available on your system's `PATH`.
    * *Note:* This tool requires features from the Conway era. Ensure your `cardano-node` and `cardano-cli` versions support the `cardano-cli conway genesis create-testnet-data` command (e.g., version 10.0.0 or later).

## Building

1.  **Update Cabal package list:**
    ```bash
    cabal update
    ```
2.  **Build the project:**
    ```bash
    cabal build launch-testnet
    ```
    This will create the executable within the `dist-newstyle` directory.

## Usage

You can run the tool using `cabal run launch-testnet -- [...]` or by executing the compiled binary directly (e.g., `$(cabal list-bin launch-testnet) [...]`).

All commands require `cardano-node` and `cardano-cli` to be accessible on your `PATH`.

```bash
$ launch-testnet 

launch-testnet - spin up or dump spec files for a local Cardano testnet

Usage: launch-testnet COMMAND

  Commands: default, dump-spec-files, custom

Available options:
  -h,--help                Show this help text

Available commands
  default                  Launch a testnet instance using built-in genesis
                           files that replicate current Mainnet settings.
  dump-spec-files          Generate local copies of the default specification
                           files (genesis, config, topology) for editing. Use
                           these modified files with the 'custom' command.
  custom                   Launch a testnet instance using custom genesis
                           specifications, node configuration, and network
                           topology provided via file paths.
```  

### 1. default  
Launch a Conway‐era single‐pool testnet in Conway era, with `0.100 s` slots and `5 min` epochs; all other Protocol Parameters, including the Constitution and Guardrails script match current Mainnet protocol paramters, excpept for `committeeMinSize` which is set to `0`. 

```shell
$ cabal run launch-testnet -- default --out-dir <DIR>
```
- Writes out under DIR:
    specs/{shelley.json, alonzo.json, conway.json}
    config.json
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
    --drep-keys 3 \
    --testnet-magic 42 \
    --out-dir <DIR>
```
- Then launches:
```
  cardano-node run \
    --config      <DIR>/config.json \
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
Only write the default spec files, config & topology into DIR:

```shell
$ cabal run launch-testnet -- dump-spec-files --out-dir <DIR>
```
You can tweak these dump spec files to change the testnet parameters and then use the `custom` command 
below to start the testnet with these custom spec files. 

### 3. custom  
Provide your own genesis spec files plus node config & topology:
```shell
$ cabal run launch-testnet -- custom \
    --shelley-spec <PATH>/shelley.json \
    --alonzo-spec  <PATH>/alonzo.json \
    --conway-spec  <PATH>/conway.json \
    --config       <PATH>/config.json \
    --topology     <PATH>/topology.json \
    --out-dir      <DIR>
```
- Validates each input file exists.  
- Copies them into DIR/specs/…, DIR/config.json and DIR/topology.json.  
- Runs the same cardano-cli conway genesis create-testnet-data … --out-dir DIR
- Launches cardano-node run against your provided config + topology, logging to DIR/node.log.

### Configuration Details & Notes

- Network Port: The cardano-node is hardcoded to run on port `6000`.
- Testnet Magic: The network uses a fixed `--testnet-magic 42`.
- `launch-testnet default` starts a local testnet with the following characteristics:
   - A Conway-era network.
   - Slot length: 0.1 seconds.
   - Epoch length: 5 minutes (3000 slots).
   - committeeMinSize: 0 (allows governance actions without active committee member votes).
   - Other parameters generally align with Cardano Mainnet defaults at the time of embedding.
   - DReps: The cardano-cli command generates keys for 3 DReps by default.
   - No Committee: The default setup does not generate committee keys (--committee-keys is omitted in the cardano-cli call).