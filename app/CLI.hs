{-# LANGUAGE OverloadedStrings #-}

module CLI (
    Command (..),
    PoolCount (..),
    TestnetMagic (..),  
    CustomPaths (..),
    optsParser,
) where

import Data.Semigroup ((<>))
import Options.Applicative
import Control.Monad (when) 
import Numeric.Natural (Natural)

-- | Record to hold file paths for the custom command
data CustomPaths = CustomPaths
    { cpShelleySpec :: FilePath
    , cpAlonzoSpec  :: FilePath
    , cpConwaySpec  :: FilePath
    , cpConfigFile  :: FilePath
    , cpTopology    :: FilePath
    , cpOutDir      :: FilePath
    } deriving (Show)

newtype PoolCount = PoolCount Int 
    deriving (Eq, Show)
newtype TestnetMagic = TestnetMagic { unTestnetMagic :: Natural }
    deriving (Eq, Show)

-- Custom reader for PoolCount that ensures the value is >= 1
poolCountReader :: ReadM PoolCount
poolCountReader = do
    n <- auto :: ReadM Int 
    when (n < 1) $ readerError "Number of pools must be 1 or greater."
    return (PoolCount n)

data Command
    = Default FilePath PoolCount TestnetMagic
    | DumpSpecs FilePath
    | Custom CustomPaths PoolCount TestnetMagic
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
                (info defaultOpts (progDesc "Launch a testnet using built-in specs. Creates N pools as specified."))
            <> command
                "dump-spec-files"
                (info dumpSpecsOpts (progDesc "Generate local copies of the default specification files for editing."))
            <> command
                "custom"
                (info customOpts (progDesc "Launch a testnet using custom spec files, config, and topology. Creates N pools as specified."))

poolsOpt :: Parser PoolCount
poolsOpt =
    option poolCountReader 
        ( long "pools"
            <> metavar "INT"
            <> value (PoolCount 1)
            <> showDefault
            <> help "Number of stake pools to create and run (must be >= 1)." 
        )

outDirOpt :: Parser FilePath
outDirOpt =
    strOption
        ( long "out-dir"
            <> metavar "DIR"
            <> help "Directory to write testnet data, logs, and config files."
        )

testnetMagicOpt :: Parser TestnetMagic
testnetMagicOpt =
    fmap TestnetMagic $ option auto
        ( long "testnet-magic"
            <> metavar "NATURAL"
            <> value 42 -- Default value if not specified
            <> showDefault
            <> help "The testnet magic number."
        )

defaultOpts :: Parser Command
defaultOpts =
    Default
        <$> outDirOpt
        <*> poolsOpt
        <*> testnetMagicOpt

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
        <*> testnetMagicOpt

