{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

import Control.Applicative (asum)
import Control.Concurrent (threadDelay)
import Control.Exception (catch, try)
import Control.Monad (MonadPlus (..), unless, void, when)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (find, isPrefixOf, isSuffixOf, tails)
import Distribution.Compat.Prelude (exitFailure, exitSuccess)
import GHC.IO.Exception (IOException (IOError))
import GHC.Stack (HasCallStack)
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (getEnv)
import System.Process (callCommand, readProcess, runCommand)

-- Globals
javaCommand jar = ["java", "-Xmx20G", "-jar", jar, "nogui"]
validJarIds = ["fabric", "forge", "minecraft_server"]

-- Utils
(.*) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.*) = (.) . (.)

(<&&>) :: (Applicative f) => f Bool -> f Bool -> f Bool
fa <&&> fb = (&&) <$> fa <*> fb

join :: (Monad m) => m (m a) -> m a
join = (id =<<)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM fb x = fb >>= flip when x

unlessM :: IO Bool -> IO () -> IO ()
unlessM = whenM . fmap not

catchToX (x :: a) = (`catch` (const (pure x) :: IOError -> IO a))

processExists name = catchToX False (True <$ readProcess "type" ["-p", name] "")

replaceIn target replacement file =
    readProcess
        "sed"
        [ "-i"
        , "s/" <> target <> "/" <> replacement <> "/"
        , file
        ]
        ""

within s = any (s `isPrefixOf`) . tails

printAndFail s = do
    putStrLn s
    mzero

sudo = callCommand . ("sudo " <>)

ls = listDirectory =<< getCurrentDirectory

serverStarted =
    any (("java" `within`) <&&> ("25565" `within`)) . lines <$> readProcess "ss" ["-tunlp"] ""

-- Main
runServer jar = do
    putStrLn "Running Server"
    callCommand (unwords $ javaCommand jar)
    putStrLn "Server Ended"

runServerInTmux jar = do
    isTmux <- catchToX False (not . null <$> getEnv "TMUX")
    if not isTmux
        then do
            putStrLn "Tmux server Starting"
            let tmux cs = readProcess "tmux" cs ""
            tmux ["new-session", "-d"]
            tmux ["send", unwords $ javaCommand jar]
            tmux ["send", "Enter"]
            putStrLn "Tmux Server Started"
            putStrLn "Run `tmux a` to attach to it"
            callCommand "tmux a"
        else
            runServer jar

installJava = do
    putStrLn "Installing Java"
    unlessM (processExists "java") . void $
        asum
            [ processExists "dnf" >> do
                sudo "dnf install -y java-latest-openjdk-headless"
            , processExists "apt" >> do
                sudo "apt update -y"
                sudo "apt install -y openjdk-16-jdk-headless"
            , processExists "pacman" >> do
                sudo "pacman install -y openjdk-16-jdk-headless"
            ]
    unlessM (processExists "java") $ printAndFail "server.hs: Java Failed to Install"
    putStrLn "Installed Java"

getValidJar = do
    files <- ls
    case find (or . traverse within validJarIds) $ filter (\s -> "jar" `isSuffixOf` s) files of
        Nothing -> printAndFail "server.hs: No jar files found"
        Just jar -> pure jar

backupServer = do
    date <- init <$> readProcess "date" ["-u", "+%Y-%m-%d_%H:%M:%S"] ""
    readProcess "zip" ["-r", "bak_world/" <> "world_" <> date, "world"] ""

-- Actual Main
main = do
    jar <- getValidJar

    serverStarted >>= \case
        False -> do
            processExists "java" >>= flip unless installJava
            files <- ls
            unless ("eula.txt" `elem` files) $ do
                runServer jar
                putStrLn "Reading the eula"
                void $ replaceIn "eula=false" "eula=true" "eula.txt"
                putStrLn "You have read the eula"
            callCommand "mkdir -p bak_world"
            backupServer
            runServerInTmux jar
            exitSuccess
        True -> do
            traverse_
                putStrLn
                [ "server has already started"
                , "check all your terminals"
                , "try running `tmux ls` to see all tmux windows"
                , "try running `tmux a` to attach to a tmux process"
                ]
            exitFailure
