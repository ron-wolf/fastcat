#!/usr/bin/env stack
-- stack runghc --resolver nightly-2017-11-14 --package shake --package hspec --install-ghc
-- --no-terminal
-- vi:syntax=hspec

import           Control.Monad     (void)
import           Development.Shake
import           Test.HUnit

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles=".shake" } $ do
    want [ "target/ac", "man/ac.1" ]

    "man/ac.1" %> \_ -> do
        need ["man/MANPAGE.md"]
        cmd ["pandoc", "man/MANPAGE.md", "-s", "-t", "man", "-o", "man/ac.1"]

    "target/hcat" %> \_ -> do
        need ["hs/cat.hs"]
        cmd ["ghc-8.2.2", "-O", "hs/cat.hs", "-o", "target/hcat"]

    "lint" ~> do
        cmd_ ["hlint", "."]
        cmd ["yamllint", ".travis.yml"]

    "target/ats-cat.tar.gz" %> \_ -> do
        need ["target/ac"]
        cmd (Cwd "target") Shell "tar caf ats-cat.tar.gz ac"

    "deploy" ~> do
        need ["target/ats-cat.tar.gz"]
        cmd ["cp", "target/ats-cat.tar.gz", "/home/vanessa/programming/rust/nessa-site/static"]

    "build" %> \_ -> do
        need ["shake.hs"]
        cmd_ ["mkdir", "-p", ".shake"]
        command_ [Cwd ".shake"] "cp" ["../shake.hs", "."]
        command [Cwd ".shake"] "ghc-8.2.2" ["-O2", "shake.hs", "-o", "../build", "-Wall", "-Werror", "-Wincomplete-uni-patterns", "-Wincomplete-record-updates"]

    "target/ac" %> \_ -> do
        ats <- getDirectoryFiles "" ["src//*.*ats"]
        need ats
        cmd_ ["mkdir", "-p", "target"]
        command_ [EchoStderr False] "atscc" (ats ++ ["-o", "target/ac", "-O3", "-mtune=native"])
        command [] "rm" ["-f", "ac_dats.c"]

    "install" ~> do
        need ["target/ac", "man/ac.1"]
        home <- getEnv "HOME"
        case home of
            Just h -> do
                cmd_ ["cp", "man/ac.1", h ++ "/.local/share/man/man1"]
                cmd ["cp", "target/ac", h ++ "/.local/bin"]
            _ -> putNormal "Warning: could not detect home directory; skipping install."

    "bench" ~> do
        need ["target/ac", "target/hcat"]
        cmd
            [ "bench"
            , "cat shake.hs"
            , "./target/ac shake.hs"
            , "./target/hcat shake.hs"
            , "ac colors | perl -pe 's/\\e\\[?.*?[\\@-~]//g'"
            , "ac colors | strip-ansi"
            , "ac colors | sed -r \"s/\x1B\\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g\""
            , "ac colors | python3 py/strip-ansi.py"
            , "ac colors | ansifilter"
            , "ac colors | ac -s colors"
            ]

    "test" ~> do
        need ["target/ac"]
        (Stdout out) <- command [] "./target/ac" ["colors"]
        expected <- liftIO $ readFile "colors"
        let test1 = TestCase (assertEqual "colors" expected out)
        void $ liftIO $ runTestTT test1

    "clean" ~> do
        removeFilesAfter "." ["//*.c", "//*.hi", "//*.o", "tags", "build", "man/ac.1", "//*.tar.gz"]
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter "target" ["//*"]
