{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CLI (Command (..), CustomPaths (..), PoolCount (..), optsParser)
import LaunchTestnet.Commons (runDumpSpecs, successText)
import LaunchTestnet.Custom (runCustom)
import LaunchTestnet.Default (runDefault)
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)

main :: IO ()
main = do
    let parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError)
    cmd <- customExecParser parserPrefs optsParser
    case cmd of
        Default outDir poolCount -> runDefault outDir poolCount
        DumpSpecs dir -> runDumpSpecs dir >> (putStrLn $ successText ("✓ Default spec files dumped successfully."))
        Custom paths poolCount -> runCustom paths poolCount
