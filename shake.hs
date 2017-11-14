#!/usr/bin/env stack
-- stack runghc --resolver nightly-2017-11-14 --package shake --package hspec --install-ghc
-- --no-terminal
-- vi:syntax=hspec

import           Control.Monad     (void)
import           Development.Shake
import           Test.HUnit

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles=".shake" } $ do
    want [ "target/ac" ]

    "target/hcat" %> \_ -> do
        need ["hs/cat.hs"]
        cmd ["ghc", "-O", "hs/cat.hs", "-o", "target/hcat"]

    "lint" ~> do
        cmd_ ["hlint", "."]
        cmd ["yamllint", ".travis.yml"]

    "target/ats-cat.tar.gz" %> \_ -> do
        need ["target/ac"]
        cmd (Cwd "target") Shell "tar caf ats-cat.tar.gz ac"

    "deploy" ~> do
        need ["target/ats-cat.tar.gz"]
        cmd ["cp", "target/ats-cat.tar.gz", "/home/vanessa/programming/rust/nessa-site/static"]

    "shake" %> \_ -> do
        need ["shake.hs"]
        cmd_ ["mkdir", "-p", ".shake"]
        command_ [Cwd ".shake"] "cp" ["../shake.hs", "."]
        command [Cwd ".shake"] "ghc" ["-O", "shake.hs", "-o", "../shake", "-Wall", "-Werror", "-Wincomplete-uni-patterns", "-Wincomplete-record-updates"]

    "target/ac" %> \_ -> do
        ats <- getDirectoryFiles "" ["src//*.*ats"]
        need ats
        cmd_ ["mkdir", "-p", "target"]
        command_ [EchoStderr False] "atscc" (ats ++ ["-o", "target/ac", "-O3"]) -- -DATS_MEMALLOC_LIBC
        command [] "rm" ["-f", "ac_dats.c"]

    "install" ~> do
        need ["target/ac"]
        home <- getEnv "HOME"
        case home of
            Just h -> cmd ["cp", "target/ac", h ++ "/.local/bin"]
            _ -> putNormal "Warning: could not detect home directory; skipping install."

    "bench" ~> do
        need ["target/ac", "target/hcat"]
        cmd ["bench", "cat shake.hs", "./target/ac shake.hs", "./target/hcat shake.hs", "ac --show-nonprinting colors", "cat --show-nonprinting colors"]

    "test" ~> do
        need ["target/ac"]
        (Stdout out) <- command [] "./target/ac" ["colors"]
        expected <- liftIO $ readFile "colors"
        let test1 = TestCase (assertEqual "colors" expected out)
        void $ liftIO $ runTestTT test1

    "clean" ~> do
        removeFilesAfter "." ["//*.c", "//*.hi", "//*.o", "tags", "shake", "//*.tar.gz"]
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter "target" ["//*"]
