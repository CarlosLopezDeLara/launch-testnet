-- app/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           CLI                         (Command(..), optsParser)
import           Control.Monad               (forM_, unless)
import           Data.Semigroup              ((<>))
import           Data.FileEmbed              (embedFile)
import qualified Data.ByteString             as BS
import           Data.List                   (intercalate)
import           Options.Applicative         (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)
import           System.Directory            (createDirectoryIfMissing, doesFileExist, copyFile)
import           System.FilePath             ((</>))
import           System.Process              (callCommand, callProcess)
import           System.IO                   (stderr, hPutStrLn)
import           System.Exit                 (exitFailure)
import GHC.Exception (divZeroException)

specShelley, specAlonzo, specConway, specTopology, specConfig :: BS.ByteString
specShelley  = $(embedFile "data/specs/shelley.json")
specAlonzo   = $(embedFile "data/specs/alonzo.json")
specConway   = $(embedFile "data/specs/conway.json")
specTopology = $(embedFile "data/specs/topology.json")
specConfig   = $(embedFile "data/specs/config.json")

main :: IO ()
main = do
  let parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError)
  cmd <- customExecParser parserPrefs optsParser
  case cmd of
    Default   dir -> runDefault dir
    DumpSpecs dir -> runDumpSpecs dir >> putStrLn "DumpSpecs done."
    Custom    shSpec alSpec cwSpec configFile topo dir -> runCustom shSpec alSpec cwSpec configFile topo dir

-- | Dump default spec files and config/topology into a directory
--   This is useful for inspecting the specs and creating a custom testnet
runDumpSpecs :: FilePath -> IO ()
runDumpSpecs dir = do
  let specsDir     = dir </> "specs"
      configPath   = dir </> "config.json"
      topologyPath = dir </> "topology.json"

  createDirectoryIfMissing True specsDir

  BS.writeFile (specsDir </> "shelley.json") specShelley
  BS.writeFile (specsDir </> "alonzo.json")  specAlonzo
  BS.writeFile (specsDir </> "conway.json")  specConway
  BS.writeFile configPath                   specConfig
  BS.writeFile topologyPath                 specTopology

  putStrLn $ "Specs, config & topology dumped to: " ++ dir

-- | Default journey. This will create a testnet with the default specs
runDefault :: FilePath -> IO ()
runDefault dir = do
  runDumpSpecs dir -- write defult specs + config/topology to disc to be used by create-testnet-data 

  callProcess "cardano-cli"
    [ "conway","genesis","create-testnet-data"
    , "--spec-shelley",     dir </> "specs/shelley.json"
    , "--spec-alonzo",      dir </> "specs/alonzo.json"
    , "--spec-conway",      dir </> "specs/conway.json"
    , "--pools","1","--stake-delegators","3"
    , "--total-supply","43000000000000"
    , "--delegated-supply","9000000000000"
    , "--drep-keys", "3" 
    , "--testnet-magic","42"
    , "--out-dir",          dir
    ]

  let quoted p = '"' : p ++ "\""
      dbPath   = dir </> "db"
      sockPath = dir </> "node.sock"
      poolDir  = dir </> "pools-keys/pool1"
      logFile  = poolDir </> "node.log"
      cmd = intercalate " "
        [ "cardano-node run"
        , "--config",                         quoted (dir </> "config.json")
        , "--topology",                       quoted (dir </> "topology.json")
        , "--database-path",                  quoted dbPath
        , "--socket-path",                    quoted sockPath
        , "--shelley-kes-key",                quoted (poolDir </> "kes.skey")
        , "--shelley-vrf-key",                quoted (poolDir </> "vrf.skey")
        , "--byron-delegation-certificate",   quoted (poolDir </> "byron-delegation.cert")
        , "--byron-signing-key",              quoted (poolDir </> "byron-delegate.key")
        , "--shelley-operational-certificate",quoted (poolDir </> "opcert.cert")
        , "--host-addr",                     "0.0.0.0"
        , "--port",                          "6000"
        , "| tee -a",                         quoted logFile
        ]
  callCommand cmd

-- Use custom spec files
runCustom
  :: FilePath   -- ^ shelleySpec
  -> FilePath   -- ^ alonzoSpec
  -> FilePath   -- ^ conwaySpec
  -> FilePath   -- ^ configFile
  -> FilePath   -- ^ topologyFile
  -> FilePath   -- ^ out-dir
  -> IO ()
runCustom shelleySpec alonzoSpec conwaySpec configFile topologyFile dir = do
  putStrLn $ "Running custom testnet in: " ++ dir

  -- Validate inputs
  forM_ [ (shelleySpec, "Shelley spec")
        , (alonzoSpec,  "Alonzo spec")
        , (conwaySpec,  "Conway spec")
        , (configFile,  "Config file")
        , (topologyFile,"Topology file")
        ] $ \(path,label) -> do
    exists <- doesFileExist path
    unless exists $ do
      hPutStrLn stderr $ label ++ " not found: " ++ path
      exitFailure

  let specsDir     = dir </> "specs"
      destShelley  = specsDir </> "shelley.json"
      destAlonzo   = specsDir </> "alonzo.json"
      destConway   = specsDir </> "conway.json"
      configDest   = dir </> "config.json"
      topologyDest = dir </> "topology.json"

  createDirectoryIfMissing True specsDir

  copyFile shelleySpec  destShelley
  copyFile alonzoSpec   destAlonzo
  copyFile conwaySpec   destConway
  copyFile configFile   configDest
  copyFile topologyFile topologyDest

  callProcess "cardano-cli"
    [ "conway","genesis","create-testnet-data"
    , "--spec-shelley",     destShelley
    , "--spec-alonzo",      destAlonzo
    , "--spec-conway",      destConway
    , "--pools","1","--stake-delegators","3"
    , "--total-supply","43000000000000"
    , "--delegated-supply","9000000000000"
        , "--drep-keys", "3" 
    , "--testnet-magic","42"
    , "--out-dir",          dir
    ]

  let quoted p = '"' : p ++ "\""
      dbPath   = dir </> "db"
      sockPath = dir </> "node.sock"
      poolDir  = dir </> "pools-keys/pool1"
      logFile  = dir </> "node.log"
      cmd = intercalate " "
        [ "cardano-node run"
        , "--config",                         quoted configDest
        , "--topology",                       quoted topologyDest
        , "--database-path",                  quoted dbPath
        , "--socket-path",                    quoted sockPath
        , "--shelley-kes-key",                quoted (poolDir </> "kes.skey")
        , "--shelley-vrf-key",                quoted (poolDir </> "vrf.skey")
        , "--byron-delegation-certificate",   quoted (poolDir </> "byron-delegation.cert")
        , "--byron-signing-key",              quoted (poolDir </> "byron-delegate.key")
        , "--shelley-operational-certificate",quoted (poolDir </> "opcert.cert")
        , "--host-addr",                     "0.0.0.0"
        , "--port",                          "6000"
        , "| tee -a",                         quoted logFile
        ]
  callCommand cmd
