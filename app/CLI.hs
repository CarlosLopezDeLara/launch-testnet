{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( Command(..)
  , optsParser
  ) where

import           Options.Applicative
import           Data.Semigroup                ((<>))

-- | Available subcommands
data Command
  = Default   FilePath
  | DumpSpecs FilePath
  | Custom    FilePath  FilePath  FilePath  FilePath  FilePath  FilePath
    -- ^ shelley.json, alonzo.json, conway.json, config.json, topology.json, out-dir
  deriving Show

optsParser :: ParserInfo Command
optsParser = info (parser <**> helper) $
     fullDesc
  <> header   "launch-testnet - spin up or dump spec files for a local Cardano testnet"
  <> progDesc "Commands: default, dump-spec-files, custom"

parser :: Parser Command
parser = hsubparser $
     commandGroup "Available commands"
  <> command "default"
       ( info defaultOpts
         ( progDesc "Launch a testnet instance using built-in genesis files that replicate current Mainnet settings." ) )
  <> command "dump-spec-files"
       ( info dumpSpecsOpts
         ( progDesc "Generate local copies of the default specification files (genesis, config, topology) for editing. Use these modified files with the 'custom' command." ) )
  <> command "custom"
       ( info customOpts
         ( progDesc "Launch a testnet instance using custom genesis specifications, node configuration, and network topology provided via file paths." ) )

defaultOpts :: Parser Command
defaultOpts = Default
  <$> strOption
      ( long "out-dir"
     <> metavar "DIR"
     <> help "Directory to write testnet data + config files" )

dumpSpecsOpts :: Parser Command
dumpSpecsOpts = DumpSpecs
  <$> strOption
      ( long "out-dir"
     <> metavar "DIR"
     <> help "Directory in which to dump the default spec files + config/topology" )

customOpts :: Parser Command
customOpts = Custom
  <$> strOption
      ( long "shelley-spec"
     <> metavar "FILE"
     <> help "Path to your custom shelley.json" )
  <*> strOption
      ( long "alonzo-spec"
     <> metavar "FILE"
     <> help "Path to your custom alonzo.json" )
  <*> strOption
      ( long "conway-spec"
     <> metavar "FILE"
     <> help "Path to your custom conway.json" )
  <*> strOption
      ( long "config"
     <> metavar "FILE"
     <> help "Path to your custom config.json" )
  <*> strOption
      ( long "topology"
     <> metavar "FILE"
     <> help "Path to your custom topology.json" )
  <*> strOption
      ( long "out-dir"
     <> metavar "DIR"
     <> help "Directory to write testnet data + config files" )
