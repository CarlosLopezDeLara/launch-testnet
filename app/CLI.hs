{-# LANGUAGE OverloadedStrings #-}

module CLI (
    Command (..),
    PoolCount (..),
    optsParser,
) where

import Data.Semigroup ((<>))
import Options.Applicative

newtype PoolCount = PoolCount Int
    deriving (Eq, Show)
data Command
    = Default FilePath PoolCount
    | DumpSpecs FilePath
    | -- | shelley.json, alonzo.json, conway.json, config.json, topology.json, out-dir
      Custom FilePath FilePath FilePath FilePath FilePath FilePath
    deriving (Show)

optsParser :: ParserInfo Command
optsParser =
    info (parser <**> helper) $
        fullDesc
            <> header "launch-testnet - spin up or dump spec files for a local Cardano testnet"
            <> progDesc "Commands: default, dump-spec-files, custom"

parser :: Parser Command
parser =
    hsubparser $
        commandGroup "Available commands"
            <> command
                "default"
                ( info
                    defaultOpts
                    (progDesc "Launch a testnet instance using built-in genesis files that replicate current Mainnet settings.")
                )
            <> command
                "dump-spec-files"
                ( info
                    dumpSpecsOpts
                    (progDesc "Generate local copies of the default specification files (genesis, config, topology) for editing. Use these modified files with the 'custom' command.")
                )
            <> command
                "custom"
                ( info
                    customOpts
                    (progDesc "Launch a testnet instance using custom genesis specifications, node configuration, and network topology provided via file paths.")
                )

poolsOpt :: Parser PoolCount
poolsOpt =
    option
        (PoolCount <$> auto) -- auto :: ReadM Int  ➜  ReadM PoolCount
        ( long "pools"
            <> metavar "INT"
            <> value (PoolCount 1) -- default = 1
            <> showDefault
            <> help "Number of stake‑pools to create / run"
        )

defaultOpts :: Parser Command
defaultOpts =
    Default
        <$> strOption
            ( long "out-dir"
                <> metavar "DIR"
                <> help "Directory to write testnet data + config files"
            )
        <*> poolsOpt

dumpSpecsOpts :: Parser Command
dumpSpecsOpts =
    DumpSpecs
        <$> strOption
            ( long "out-dir"
                <> metavar "DIR"
                <> help "Directory in which to dump the default spec files + config/topology"
            )

customOpts :: Parser Command
customOpts =
    Custom
        <$> strOption
            ( long "shelley-spec"
                <> metavar "FILE"
                <> help "Path to your custom shelley.json"
            )
        <*> strOption
            ( long "alonzo-spec"
                <> metavar "FILE"
                <> help "Path to your custom alonzo.json"
            )
        <*> strOption
            ( long "conway-spec"
                <> metavar "FILE"
                <> help "Path to your custom conway.json"
            )
        <*> strOption
            ( long "config"
                <> metavar "FILE"
                <> help "Path to your custom config.json"
            )
        <*> strOption
            ( long "topology"
                <> metavar "FILE"
                <> help "Path to your custom topology.json"
            )
        <*> strOption
            ( long "out-dir"
                <> metavar "DIR"
                <> help "Directory to write testnet data + config files"
            )
