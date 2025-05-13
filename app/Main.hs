{-# LANGUAGE OverloadedStrings #-}

module Main where

import CLI (Command (..), optsParser) 
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)

import LaunchTestnet.Commons (runDumpSpecs, successText) 
import LaunchTestnet.Custom (runCustom)
import LaunchTestnet.Default (runDefault)

main :: IO ()
main = do
    let parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError)
    cmd <- customExecParser parserPrefs optsParser
    case cmd of
        Default outDir poolCount magic -> runDefault outDir poolCount magic
        DumpSpecs dir                  -> runDumpSpecs dir >> (putStrLn $ successText ("✓ Default spec files dumped successfully."))
        Custom paths poolCount magic   -> runCustom paths poolCount magic