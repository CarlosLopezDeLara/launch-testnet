{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LaunchTestnet.Commons (
    -- Data Types
    CreateTestnetDataArgs(..),
    SpawnNodesArgs(..),


    -- Embedded Specs
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
    printEnvInstructions,

    -- Color/Formatting Helpers
    path,
    poolId,
    successText 
) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.List (intercalate)
import System.FilePath ((</>), takeFileName)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.IO (Handle, IOMode(AppendMode), openFile, hClose, stderr, hPutStrLn)
import System.Process (CreateProcess (..), StdStream (..), proc, createProcess, waitForProcess, ProcessHandle)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import Control.Monad (unless, forM_, filterM, forM, when) 
import Control.Concurrent.Async (mapConcurrently) 
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import System.Console.ANSI 
import CLI (PoolCount(..)) 

-- ANSI escape codes for colors
blue, cyan, green, reset :: String
blue = setSGRCode [SetColor Foreground Vivid Blue]
cyan = setSGRCode [SetColor Foreground Vivid Cyan]
green = setSGRCode [SetColor Foreground Vivid Green]
reset = setSGRCode [Reset] 

-- Helper to colorize paths
path :: FilePath -> String
path p = cyan ++ p ++ reset

-- Helper to colorize pool numbers/IDs
poolId :: String -> String
poolId pidText = blue ++ pidText ++ reset

-- Helper for success messages
successText :: String -> String
successText s = green ++ s ++ reset


-- | Embed the default spec/config/topology files
specShelley, specAlonzo, specConway, specTopology, specConfig :: BS.ByteString
specShelley = $(embedFile "data/specs/shelley.json")
specAlonzo = $(embedFile "data/specs/alonzo.json")
specConway = $(embedFile "data/specs/conway.json")
specTopology = $(embedFile "data/specs/topology.json")
specConfig = $(embedFile "data/specs/config.json")

-- | Arguments for creating testnet data
data CreateTestnetDataArgs = CreateTestnetDataArgs
    { ctaSpecShelley    :: FilePath
    , ctaSpecAlonzo     :: FilePath
    , ctaSpecConway     :: FilePath
    , ctaOutDir         :: FilePath
    , ctaPoolCount      :: PoolCount
    , ctaRelaysFile     :: Maybe FilePath
    , ctaTestnetMagic   :: Word
    , ctaTotalSupply    :: Word
    , ctaDelegatedSupply:: Word
    , ctaDrepKeys       :: Int
    , ctaStakeDelegators:: Int
    }

-- | Common arguments for spawning cardano-node instances
data SpawnNodesArgs = SpawnNodesArgs
    { snaConfigPath   :: FilePath
    , snaTopologyPath :: FilePath
    , snaOutDir       :: FilePath
    , snaPoolCount    :: PoolCount
    , snaBasePort     :: Int
    }

runCliProcess :: String -> [String] -> String -> IO ()
runCliProcess cmd cliArgs desc = do
    putStrLn $ "Running: " ++ cmd ++ " " ++ unwords cliArgs
    (_, _, _, pHandle) <- createProcess (proc cmd cliArgs)
    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $ do
        hPutStrLn stderr $ "Error: " ++ desc ++ " failed with exit code: " ++ show exitCode
        exitWith exitCode
    putStrLn $ successText ("✓ " ++ desc ++ " completed successfully.")

createTestnetData :: CreateTestnetDataArgs -> IO ()
createTestnetData args@CreateTestnetDataArgs{..} = do
    let (PoolCount nPools) = ctaPoolCount
    let commonArgs =
            [ "conway", "genesis", "create-testnet-data"
            , "--spec-shelley", ctaSpecShelley
            , "--spec-alonzo", ctaSpecAlonzo
            , "--spec-conway", ctaSpecConway
            , "--pools", show nPools
            , "--stake-delegators", show ctaStakeDelegators
            , "--total-supply", show ctaTotalSupply
            , "--delegated-supply", show ctaDelegatedSupply
            , "--drep-keys", show ctaDrepKeys
            , "--testnet-magic", show ctaTestnetMagic
            , "--out-dir", ctaOutDir
            ]
        relayArgsList = case ctaRelaysFile of
                        Just relays -> ["--relays", relays]
                        Nothing     -> []
    runCliProcess "cardano-cli" (commonArgs ++ relayArgsList) "Create Testnet Data"

generateRelayFile :: FilePath -> PoolCount -> Int -> IO (Maybe FilePath)
generateRelayFile outDir poolCount@(PoolCount nPools) basePort = do
    if nPools > 1
        then do
            let relaysPathFile = outDir </> "pool-relays.json" -- Variable renamed to avoid clash
            let entries =
                    [ "\""
                        <> show (idx - 1)
                        <> "\": [ \
                           \  { \"single host address\": { \
                           \    \"IPv4\": \"127.0.0.1\", \
                           \    \"IPv6\": null, \
                           \    \"port\": "
                        <> show (basePort + (idx - 1))
                        <> " } } ]"
                    | idx <- [1 .. nPools]
                    ]
                relaysContent = "{\n" <> intercalate ",\n" entries <> "\n}\n"
            writeFile relaysPathFile relaysContent
            putStrLn $ "→ Wrote multi-pool relays config to: " ++ path relaysPathFile
            return (Just relaysPathFile)
        else return Nothing

spawnNodes :: SpawnNodesArgs -> IO ()
spawnNodes args@SpawnNodesArgs{..} = do
    let (PoolCount nPools) = snaPoolCount
    let baseOutDir = snaOutDir

    putStrLn $ "Attempting to spawn " ++ show nPools ++ " node(s) using base output directory: " ++ path baseOutDir
    putStrLn $ "Node Config file to be used by all nodes: " ++ path snaConfigPath
    putStrLn $ "Node Topology file to be used by all nodes: " ++ path snaTopologyPath

    let poolIndices = [1 .. nPools]

    -- STEP 1: Sequential loop for ordered print statements and pre-flight checks
    nodeLaunchConfigs <- forM poolIndices $ \i -> do
        let pIdOutput = poolId ("[Pool " ++ show i ++ "]")

        let relPoolDirName = "pool" <> show i
        let relPoolKeysPath = "pools-keys" </> relPoolDirName
        let relDbPathForNode = "db" </> relPoolDirName
        let relSockPathForNode = "node" <> show i <> ".sock"

        let fullPoolSpecificKeyDir = baseOutDir </> relPoolKeysPath
        let fullDbPathFromLaunchTestnet = baseOutDir </> relDbPathForNode
        let fullLogFilePath = fullPoolSpecificKeyDir </> "node.log"
        let nodePort = snaBasePort + (i - 1)

        putStrLn $ "\n" ++ pIdOutput ++ " Preparing to launch..."
        putStrLn $ pIdOutput ++ " Key/OpCert Dir (relative to node CWD): " ++ path relPoolKeysPath
        putStrLn $ pIdOutput ++ " DB Path (relative to node CWD): " ++ path relDbPathForNode
        putStrLn $ pIdOutput ++ " Socket Path (relative to node CWD): " ++ path relSockPathForNode
        putStrLn $ pIdOutput ++ " Log File Path (full): " ++ path fullLogFilePath
        putStrLn $ pIdOutput ++ " Node Port: " ++ show nodePort

        putStrLn $ pIdOutput ++ " Ensuring directories exist (using full paths)..."
        createDirectoryIfMissing True fullPoolSpecificKeyDir
        createDirectoryIfMissing True fullDbPathFromLaunchTestnet
        putStrLn $ pIdOutput ++ successText " ✓ Directories ensured."

        let fullKesKeyPath = fullPoolSpecificKeyDir </> "kes.skey"
            fullVrfKeyPath = fullPoolSpecificKeyDir </> "vrf.skey"
            fullOpCertPath = fullPoolSpecificKeyDir </> "opcert.cert"
            fullByronDeleCertPath = fullPoolSpecificKeyDir </> "byron-delegation.cert"
            fullByronSignKeyPath  = fullPoolSpecificKeyDir </> "byron-delegate.key"

        putStrLn $ pIdOutput ++ " Verifying existence of critical key files (using full paths)..."
        let criticalFilesToVerify = [ (fullKesKeyPath, "KES Key"), (fullVrfKeyPath, "VRF Key"), (fullOpCertPath, "Operational Certificate")]
        missingFiles <- filterM (fmap not . doesFileExist . fst) criticalFilesToVerify
        unless (null missingFiles) $ do
            forM_ missingFiles $ \(fp, fname) ->
                hPutStrLn stderr $ pIdOutput ++ " FATAL ERROR: Required file (" ++ fname ++ ") does not exist: " ++ path fp
            fail $ "Missing critical file(s) for pool " ++ show i

        putStrLn $ pIdOutput ++ successText " ✓ All critical key files verified."

        let nodeRunArgs =
                [ "run"
                , "--config", takeFileName snaConfigPath
                , "--topology", takeFileName snaTopologyPath
                , "--database-path", relDbPathForNode
                , "--socket-path", relSockPathForNode
                , "--shelley-kes-key", relPoolKeysPath </> "kes.skey"
                , "--shelley-vrf-key", relPoolKeysPath </> "vrf.skey"
                , "--byron-delegation-certificate", relPoolKeysPath </> "byron-delegation.cert"
                , "--byron-signing-key", relPoolKeysPath </> "byron-delegate.key"
                , "--shelley-operational-certificate", relPoolKeysPath </> "opcert.cert"
                , "--host-addr", "0.0.0.0"
                , "--port", show nodePort
                ]
        
        putStrLn $ pIdOutput ++ " Constructing command: cardano-node " ++ unwords nodeRunArgs
        let processSpec = (proc "cardano-node" nodeRunArgs) { std_err = Inherit, cwd = Just baseOutDir }
        putStrLn $ pIdOutput ++ " Intended CWD for node process: " ++ path baseOutDir
        
        return (pIdOutput, fullLogFilePath, processSpec) 

    -- STEP 2: Concurrent loop for actual node launching
    putStrLn "\nAttempting to launch node processes concurrently..."
    results <- mapConcurrently (\(pIdOutput, logPathFile, spec) -> do 
        eLogHandle <- try (openFile logPathFile AppendMode) :: IO (Either IOException Handle)
        case eLogHandle of
            Left ex -> do
                hPutStrLn stderr $ pIdOutput ++ " FATAL: Error opening log file " ++ path logPathFile ++ ". Exception: " ++ show ex
                return $ Left $ "Log file open error for " ++ pIdOutput
            Right logHandle -> do
                putStrLn $ pIdOutput ++ " Log file " ++ path logPathFile ++ " opened successfully."
                let finalProcessSpec = spec { std_out = UseHandle logHandle }
                
                eProcess <- try (createProcess finalProcessSpec) :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
                case eProcess of
                    Left exCreate -> do
                        hPutStrLn stderr $ pIdOutput ++ " FATAL: Failed to create cardano-node process. Exception: " ++ show exCreate
                        hClose logHandle
                        return $ Left $ "Process creation error for " ++ pIdOutput
                    Right (_, _, _, _) -> do
                        putStrLn $ pIdOutput ++ successText " ✓ Successfully created cardano-node process."
                        putStrLn $ pIdOutput ++ " Node output directed to: " ++ path logPathFile
                        putStrLn $ pIdOutput ++ " Node errors (stderr) should appear on this console."
                        return $ Right ()
        ) nodeLaunchConfigs 
    
    let numPoolsToLaunch = length poolIndices
    let successfulLaunches = length [r | Right r <- results]
    if successfulLaunches == numPoolsToLaunch
        then putStrLn $ "\n" ++ successText ("✓ All " ++ show successfulLaunches ++ " node processes launched successfully.")
        else hPutStrLn stderr $ "\nWarning: Only " ++ show successfulLaunches ++ " out of " ++ show numPoolsToLaunch ++ " nodes were successfully launched. Check logs and errors above."

printEnvInstructions :: FilePath -> PoolCount -> Word -> IO ()
printEnvInstructions outDir (PoolCount numPools) testnetMagic = do
    absOutDir <- makeAbsolute outDir
    let title = "--- Testnet Environment Variables (Example) ---"
        separator = replicate (length title) '-'

    putStrLn "" 
    putStrLn $ successText title
    putStrLn "To interact with the testnet using cardano-cli, you might want to set:"

    -- Since numPools is guaranteed to be >= 1 by poolCountReader:
    let firstSocketRelPath = "node" <> show (1 :: Int) <> ".sock"
    let firstSocketAbsPath = absOutDir </> firstSocketRelPath
    putStrLn $ ""
    putStrLn $ "  export CARDANO_NODE_SOCKET_PATH=" ++ path firstSocketAbsPath

    when (numPools > 1) $ do
        putStrLn "" 
        putStrLn "# For other nodes, adjust the socket path (e.g., node2.sock, etc.):"
        -- Optionally, loop and print for all (this part can remain as is):
        forM_ [2 .. numPools] $ \i -> do
             let otherSocketRelPath = "node" <> show i <> ".sock"
             let otherSocketAbsPath = absOutDir </> otherSocketRelPath
             putStrLn $ "# export CARDANO_NODE_SOCKET_PATH=" ++ path otherSocketAbsPath

    putStrLn $ "  export CARDANO_NODE_NETWORK_ID=" ++ show testnetMagic
    putStrLn ""
    putStrLn $ "# (Alternatively, use the --testnet-magic " ++ show testnetMagic ++ reset ++ " flag with cardano-cli commands)"
    putStrLn ""
    putStrLn $ successText separator


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
    putStrLn $ "Default specs, config & topology dumped to: " ++ path dir
    return (shelleyPath, alonzoPath, conwayPath, configPath, topologyPath)