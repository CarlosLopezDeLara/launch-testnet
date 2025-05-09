{-# LANGUAGE OverloadedStrings #-}

module CLI (
    Command (..),
    PoolCount (..),
    CustomPaths (..), -- Export CustomPaths
    optsParser,
) where

import Data.Semigroup ((<>))
import Options.Applicative

newtype PoolCount = PoolCount Int
    deriving (Eq, Show)

-- | Record to hold file paths for the custom command
data CustomPaths = CustomPaths
    { cpShelleySpec :: FilePath
    , cpAlonzoSpec :: FilePath
    , cpConwaySpec :: FilePath
    , cpConfigFile :: FilePath
    , cpTopology :: FilePath
    , cpOutDir :: FilePath
    }
    deriving (Show)

data Command
    = Default FilePath PoolCount
    | DumpSpecs FilePath
    | Custom CustomPaths PoolCount -- Modified: Use CustomPaths and add PoolCount
    deriving (Show)

optsParser :: ParserInfo Command
optsParser =
    info (parser <**> helper) $
        fullDesc
            <> header "launch-testnet - spin up or dump spec files for a local Cardano testnet"
            <> progDesc "Commands: default, dump-spec-files, custom. All commands require cardano-node and cardano-cli in PATH."

parser :: Parser Command
parser =
    hsubparser $
        commandGroup "Available commands"
            <> command
                "default"
                ( info
                    defaultOpts
                    (progDesc "Launch a testnet using built-in specs. Creates N pools as specified.")
                )
            <> command
                "dump-spec-files"
                ( info
                    dumpSpecsOpts
                    (progDesc "Generate local copies of the default specification files for editing. Use these with the 'custom' command.")
                )
            <> command
                "custom"
                ( info
                    customOpts
                    (progDesc "Launch a testnet using custom spec files, config, and topology. Creates N pools as specified.")
                )

poolsOpt :: Parser PoolCount
poolsOpt =
    option
        (PoolCount <$> auto)
        ( long "pools"
            <> metavar "INT"
            <> value (PoolCount 1)
            <> showDefault
            <> help "Number of stake pools to create and run."
        )

outDirOpt :: Parser FilePath
outDirOpt =
    strOption
        ( long "out-dir"
            <> metavar "DIR"
            <> help "Directory to write testnet data, logs, and config files."
        )

defaultOpts :: Parser Command
defaultOpts =
    Default
        <$> outDirOpt
        <*> poolsOpt

dumpSpecsOpts :: Parser Command
dumpSpecsOpts =
    DumpSpecs
        <$> outDirOpt

customPathsOpts :: Parser CustomPaths
customPathsOpts =
    CustomPaths
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
                <> help "Path to your custom config.json node configuration file."
            )
        <*> strOption
            ( long "topology"
                <> metavar "FILE"
                <> help "Path to your custom topology.json network topology file."
            )
        <*> outDirOpt -- Reusing outDirOpt for the output directory in custom mode as well

customOpts :: Parser Command
customOpts =
    Custom
        <$> customPathsOpts
        <*> poolsOpt -- Added poolsOpt for custom command
