# launch-testnet

A Haskell CLI tool for quickly setting up local, configurable Cardano Conway-era testnets with one or more nodes. Ideal for developers.

## Prerequisites

* **GHC:** Version 9.0+ recommended.
* **Cabal:** Version 3.0+
* **Cardano Binaries:** `cardano-node` and `cardano-cli` (Conway-era, e.g., v8.9.0+) in your `PATH`.
* **Build Tools:** `gcc` or equivalent.
* **(Optional)** `bash-completion` for command-line autocompletion.

## Building

1.  `cabal update`
2.  `cabal build launch-testnet`
    (Executable at `$(cabal list-bin launch-testnet)`)

## Usage Overview

Run with `cabal run launch-testnet -- [...]` or directly if in `PATH`.
All commands require `cardano-node` and `cardano-cli` in your `PATH`.

**General Options (for `default` and `custom` commands):**
* `--out-dir DIR`: **(Required)** Output directory for all testnet data.
* `--pools INT`: Number of stake pools (nodes) to run (>=1, default: 1).
* `--testnet-magic NATURAL`: Testnet magic number (default: 42).

```bash
$ launch-testnet --help # For full command list and options
```
#### `default` Command. 

 Launches a testnet using embedded default Cardano specifications.
 
Syntax: `launch-testnet default --out-dir <DIR> [--pools <N>] [--testnet-magic <MAGIC>]`


Example:

```
cabal run launch-testnet -- default --out-dir ./my-testnet --pools 2
```

#### `dump-spec-files` Command:

Generates local copies of the default specification files, node config, and topology. Useful for customizing before using the custom command.

Syntax: `launch-testnet dump-spec-files --out-dir <DIR>`

Example:

```
cabal run launch-testnet -- dump-spec-files --out-dir ./custom-configs
```

#### `custom` Command 

Launches a testnet using user-provided genesis specifications, node configuration, and topology files.

Syntax: `launch-testnet custom \
    --shelley-spec <PATH_SHELLEY_JSON> \
    --alonzo-spec <PATH_ALONZO_JSON> \
    --conway-spec <PATH_CONWAY_JSON> \
    --config <PATH_CONFIG_JSON> \
    --topology <PATH_TOPOLOGY_JSON> \
    --out-dir <DIR> \
    [--pools <N>] \
    [--testnet-magic <MAGIC>]`

Example:

```
cabal run launch-testnet -- custom \
    --shelley-spec ./custom-configs/specs/shelley.json \
    --alonzo-spec  ./custom-configs/specs/alonzo.json \
    --conway-spec  ./custom-configs/specs/conway.json \
    --config       ./custom-configs/config.json \
    --topology     ./custom-configs/topology.json \
    --out-dir      ./my-custom-net \
    --pools 3 --testnet-magic 789
```

### Defaults Parameters:
```
- Era: Conway.
- Slot Length: 0.3 seconds.
- Epoch Length: 2000 slots (10-minute epochs).
- Committee Min Size: 0.
- DReps: 3 (default from cardano-cli).
- Node Ports: Start at 6000, incrementing for each pool.
- Testnet Magic: Default 42, configurable via --testnet-magic.
```
### Interacting with the TestnetAfter launch,

`launch-testnet` suggests environment variables:

```
--- Testnet Environment Variables (Example) ---
# For the first node (pool1):
export CARDANO_NODE_SOCKET_PATH=/full/path/to/your/out-dir/node1.sock
export CARDANO_NODE_NETWORK_ID=42
# (Or use --testnet-magic 42 with cardano-cli)
-------------------------------------
```
With env vars set:
```
cardano-cli query tip
```

With flags:
```
cardano-cli query tip --socket-path /full/path/to/out-dir/node1.sock --testnet-magic 42
```