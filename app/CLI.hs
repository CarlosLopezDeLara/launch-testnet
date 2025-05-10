{-# LANGUAGE OverloadedStrings #-}

module CLI (
    Command (..),
    PoolCount (..),
    CustomPaths (..),
    optsParser,
) where

import Data.Semigroup ((<>))
import Options.Applicative

-- import Options.Applicative.BashCompletion -- REMOVE THIS LINE or comment it out

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

newtype PoolCount = PoolCount Int
    deriving (Eq, Show)

data Command
    = Default FilePath PoolCount
    | DumpSpecs FilePath
    | Custom CustomPaths PoolCount
    deriving (Show)

optsParser :: ParserInfo Command
optsParser =
    info (parser <**> helper) $ -- REVERTED: Removed <**> completionParser
        fullDesc
            <> header "launch-testnet - spin up or dump spec files for a local Cardano testnet"
            <> progDesc "Commands: default, dump-spec-files, custom. All commands require cardano-node and cardano-cli in PATH."

-- where
-- completionParser = bashCompletionParser parser (progDesc "Generate bash completion script for launch-testnet") -- REMOVE THIS SECTION

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

customPathsParser :: Parser CustomPaths
customPathsParser =
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
        <*> outDirOpt

customOpts :: Parser Command
customOpts =
    Custom
        <$> customPathsParser
        <*> poolsOpt
