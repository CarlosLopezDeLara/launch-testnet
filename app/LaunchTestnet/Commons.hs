{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module LaunchTestnet.Commons (
    -- Data Types
    CreateTestnetDataArgs (..),
    SpawnNodesArgs (..),
    PoolCount (..),
    CustomPaths (..),
    -- Embedded Specs (or functions to access them)
    specShelley,
    specAlonzo,
    specConway,
    specTopology,
    specConfig,
    -- Helper Functions
    runCliProcess,
    createTestnetData,
    spawnNodes,
    generateRelayFile,
    runDumpSpecs,
) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.List (intercalate)

-- Ensure all necessary imports from base and other libraries are here

import CLI (CustomPaths (..), PoolCount (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (IOException, try)
import Control.Monad (filterM, forM_, unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.FilePath (takeFileName, (</>))
import System.IO (Handle, IOMode (AppendMode), hClose, hPutStrLn, openFile, stderr)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, waitForProcess)

-- | Embed the default spec/config/topology files
specShelley, specAlonzo, specConway, specTopology, specConfig :: BS.ByteString
specShelley = $(embedFile "data/specs/shelley.json")
specAlonzo = $(embedFile "data/specs/alonzo.json")
specConway = $(embedFile "data/specs/conway.json")
specTopology = $(embedFile "data/specs/topology.json")
specConfig = $(embedFile "data/specs/config.json")

-- | Arguments for creating testnet data
data CreateTestnetDataArgs = CreateTestnetDataArgs
    { ctaSpecShelley :: FilePath
    , ctaSpecAlonzo :: FilePath
    , ctaSpecConway :: FilePath
    , ctaOutDir :: FilePath
    , ctaPoolCount :: PoolCount
    , ctaRelaysFile :: Maybe FilePath
    , ctaTestnetMagic :: Word
    , ctaTotalSupply :: Word
    , ctaDelegatedSupply :: Word
    , ctaDrepKeys :: Int
    , ctaStakeDelegators :: Int
    }

-- | Common arguments for spawning cardano-node instances
data SpawnNodesArgs = SpawnNodesArgs
    { snaConfigPath :: FilePath
    , snaTopologyPath :: FilePath
    , snaOutDir :: FilePath
    , snaPoolCount :: PoolCount
    , snaBasePort :: Int
    }

-- | Helper to run a process and wait for it, exiting on failure
runCliProcess :: String -> [String] -> String -> IO ()
runCliProcess cmd args desc = do
    putStrLn $ "Running: " ++ cmd ++ " " ++ unwords args
    (_, _, _, pHandle) <- createProcess (proc cmd args)
    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $ do
        hPutStrLn stderr $ "Error: " ++ desc ++ " failed with exit code: " ++ show exitCode
        exitWith exitCode
    putStrLn $ "✓ " ++ desc ++ " completed successfully."

-- | Create testnet data using cardano-cli
createTestnetData :: CreateTestnetDataArgs -> IO ()
createTestnetData args@CreateTestnetDataArgs{..} = do
    let (PoolCount nPools) = ctaPoolCount
    let commonArgs =
            [ "conway"
            , "genesis"
            , "create-testnet-data"
            , "--spec-shelley"
            , ctaSpecShelley
            , "--spec-alonzo"
            , ctaSpecAlonzo
            , "--spec-conway"
            , ctaSpecConway
            , "--pools"
            , show nPools
            , "--stake-delegators"
            , show ctaStakeDelegators
            , "--total-supply"
            , show ctaTotalSupply
            , "--delegated-supply"
            , show ctaDelegatedSupply
            , "--drep-keys"
            , show ctaDrepKeys
            , "--testnet-magic"
            , show ctaTestnetMagic
            , "--out-dir"
            , ctaOutDir
            ]
        relayArgsList = case ctaRelaysFile of
            Just relays -> ["--relays", relays]
            Nothing -> []
    runCliProcess "cardano-cli" (commonArgs ++ relayArgsList) "Create Testnet Data"

-- Helper function to generate the pool-relays.json file
generateRelayFile :: FilePath -> PoolCount -> Int -> IO (Maybe FilePath)
generateRelayFile outDir poolCount@(PoolCount nPools) basePort = do
    if nPools > 1
        then do
            let relaysPath = outDir </> "pool-relays.json"
            let entries =
                    [ "\""
                        <> show (i - 1)
                        <> "\": [ \
                           \  { \"single host address\": { \
                           \    \"IPv4\": \"127.0.0.1\", \
                           \    \"IPv6\": null, \
                           \    \"port\": "
                        <> show (basePort + (i - 1))
                        <> " } } ]"
                    | i <- [1 .. nPools]
                    ]
                relaysContent = "{\n" <> intercalate ",\n" entries <> "\n}\n"
            writeFile relaysPath relaysContent
            putStrLn $ "→ Wrote multi-pool relays config to: " <> relaysPath
            return (Just relaysPath)
        else return Nothing

-- | Spawn N cardano-node processes
spawnNodes :: SpawnNodesArgs -> IO ()
spawnNodes args@SpawnNodesArgs{..} = do
    let (PoolCount nPools) = snaPoolCount
    let baseOutDir = snaOutDir

    putStrLn $ "Attempting to spawn " ++ show nPools ++ " node(s) using base output directory: " ++ baseOutDir
    putStrLn $ "Node Config file to be used by all nodes: " ++ snaConfigPath
    putStrLn $ "Node Topology file to be used by all nodes: " ++ snaTopologyPath

    forConcurrently_ [1 .. nPools] $ \i -> do
        let relPoolDirName = "pool" <> show i
        let relPoolKeysPath = "pools-keys" </> relPoolDirName
        let relDbPathForNode = "db" </> relPoolDirName
        let relSockPathForNode = "node" <> show i <> ".sock"

        let fullPoolSpecificKeyDir = baseOutDir </> relPoolKeysPath
        let fullDbPathFromLaunchTestnet = baseOutDir </> relDbPathForNode
        let fullLogFilePath = fullPoolSpecificKeyDir </> "node.log"

        let nodePort = snaBasePort + (i - 1)

        putStrLn $ "\n[Pool " ++ show i ++ "] Preparing to launch..."
        putStrLn $ "[Pool " ++ show i ++ "] Key/OpCert Dir (relative to node CWD): " ++ relPoolKeysPath
        putStrLn $ "[Pool " ++ show i ++ "] DB Path (relative to node CWD): " ++ relDbPathForNode
        putStrLn $ "[Pool " ++ show i ++ "] Socket Path (relative to node CWD): " ++ relSockPathForNode
        putStrLn $ "[Pool " ++ show i ++ "] Log File Path (full): " ++ fullLogFilePath
        putStrLn $ "[Pool " ++ show i ++ "] Node Port: " ++ show nodePort

        putStrLn $ "[Pool " ++ show i ++ "] Ensuring directories exist (using full paths)..."
        createDirectoryIfMissing True fullPoolSpecificKeyDir
        createDirectoryIfMissing True fullDbPathFromLaunchTestnet
        putStrLn $ "[Pool " ++ show i ++ "] Directories ensured."

        let fullKesKeyPath = fullPoolSpecificKeyDir </> "kes.skey"
            fullVrfKeyPath = fullPoolSpecificKeyDir </> "vrf.skey"
            fullOpCertPath = fullPoolSpecificKeyDir </> "opcert.cert"
            fullByronDeleCertPath = fullPoolSpecificKeyDir </> "byron-delegation.cert"
            fullByronSignKeyPath = fullPoolSpecificKeyDir </> "byron-delegate.key"

        putStrLn $ "[Pool " ++ show i ++ "] Verifying existence of critical key files (using full paths)..."
        let criticalFilesToVerify = [(fullKesKeyPath, "KES Key"), (fullVrfKeyPath, "VRF Key"), (fullOpCertPath, "Operational Certificate")]
        missingFiles <- filterM (fmap not . doesFileExist . fst) criticalFilesToVerify
        unless (null missingFiles) $ do
            forM_ missingFiles $ \(fp, name) ->
                hPutStrLn stderr $ "[Pool " ++ show i ++ "] FATAL ERROR: Required file (" ++ name ++ ") does not exist: " ++ fp
            fail $ "Missing critical file(s) for pool " ++ show i

        putStrLn $ "[Pool " ++ show i ++ "] All critical key files verified."

        let nodeRunArgs =
                [ "run"
                , "--config"
                , takeFileName snaConfigPath
                , "--topology"
                , takeFileName snaTopologyPath
                , "--database-path"
                , relDbPathForNode
                , "--socket-path"
                , relSockPathForNode
                , "--shelley-kes-key"
                , relPoolKeysPath </> "kes.skey"
                , "--shelley-vrf-key"
                , relPoolKeysPath </> "vrf.skey"
                , "--byron-delegation-certificate"
                , relPoolKeysPath </> "byron-delegation.cert"
                , "--byron-signing-key"
                , relPoolKeysPath </> "byron-delegate.key"
                , "--shelley-operational-certificate"
                , relPoolKeysPath </> "opcert.cert"
                , "--host-addr"
                , "0.0.0.0"
                , "--port"
                , show nodePort
                ]
        putStrLn $ "[Pool " ++ show i ++ "] Constructing command: cardano-node " ++ unwords nodeRunArgs
        let processSpec = (proc "cardano-node" nodeRunArgs){std_err = Inherit, cwd = Just baseOutDir}
        putStrLn $ "[Pool " ++ show i ++ "] Intended CWD for node process: " ++ baseOutDir
        eLogHandle <- try (openFile fullLogFilePath AppendMode) :: IO (Either IOException Handle)
        case eLogHandle of
            Left ex -> hPutStrLn stderr $ "[Pool " ++ show i ++ "] FATAL: Error opening log file " ++ fullLogFilePath ++ ". Exception: " ++ show ex
            Right logHandle -> do
                putStrLn $ "[Pool " ++ show i ++ "] Log file " ++ fullLogFilePath ++ " opened successfully."
                let finalProcessSpec = processSpec{std_out = UseHandle logHandle}

                eProcess <- try (createProcess finalProcessSpec) :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
                case eProcess of
                    Left exCreate -> do
                        hPutStrLn stderr $ "[Pool " ++ show i ++ "] FATAL: Failed to create cardano-node process. Exception: " ++ show exCreate
                        hClose logHandle
                    Right (_, _, _, _) -> do
                        putStrLn $ "[Pool " ++ show i ++ "] ✓ Successfully created cardano-node process."
                        putStrLn $ "[Pool " ++ show i ++ "] Node output directed to: " ++ fullLogFilePath
                        putStrLn $ "[Pool " ++ show i ++ "] Node errors (stderr) should appear on this console."

-- | Dump default spec files and config/topology into a directory
runDumpSpecs :: FilePath -> IO (FilePath, FilePath, FilePath, FilePath, FilePath)
runDumpSpecs dir = do
    let specsDir = dir </> "specs"
        shelleyPath = specsDir </> "shelley.json"
        alonzoPath = specsDir </> "alonzo.json"
        conwayPath = specsDir </> "conway.json"
        configPath = dir </> "config.json"
        topologyPath = dir </> "topology.json"

    createDirectoryIfMissing True specsDir

    BS.writeFile shelleyPath specShelley
    BS.writeFile alonzoPath specAlonzo
    BS.writeFile conwayPath specConway
    BS.writeFile configPath specConfig
    BS.writeFile topologyPath specTopology

    putStrLn $ "Default specs, config & topology dumped to: " ++ dir
    return (shelleyPath, alonzoPath, conwayPath, configPath, topologyPath)
