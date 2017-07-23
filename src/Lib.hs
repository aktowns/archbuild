{-# LANGUAGE RecordWildCards #-}
module Lib where

import           Control.Concurrent  (forkIO)
import           Control.Monad.Loops (whileM_)
import           Data.List           (isPrefixOf)
import           Data.Maybe          (isNothing)
import           GHC.IO.Handle       (Handle, hGetLine, hIsEOF)
import           System.Directory    (createDirectory, doesDirectoryExist)
import           System.Exit         (ExitCode (..))
import           System.Posix.Temp   (mkdtemp)
import           System.Process      (StdStream (..), createProcess, cwd, proc,
                                      std_err, std_out, waitForProcess)


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace a b xx@(x:xs) =
    if isPrefixOf a xx then b ++ replace a b (drop (length a) xx)
                       else x : replace a b xs

data Package = Package { packageName :: String
                       , packageUrl  :: String
                       } deriving (Show, Eq, Ord)

data BuildStatus = BuildOk | BuildFailed String deriving (Show, Eq, Ord)

data PackageStatus = PackageStatus { package :: Package
                                   , status  :: BuildStatus } deriving (Show, Eq, Ord)

execute :: FilePath -> [String] -> IO ExitCode
execute app args = do
    (_, Just stdout, Just stderr, ph) <- createProcess $ (proc app args) { cwd = Just "/home/akt/"
                                                                         , std_out = CreatePipe
                                                                         , std_err = CreatePipe }
    forkIO $
        whileM_ (not <$> hIsEOF stdout) $ do
            line <- hGetLine stdout
            putStrLn $ "DEBUG (stdout): " ++ line

    forkIO $
        whileM_ (not <$> hIsEOF stderr) $ do
            line <- hGetLine stderr
            putStrLn $ "DEBUG (stderr): " ++ line

    code <- waitForProcess ph
    putStrLn $ "DEBUG (exit code): " ++ show code
    return code

bootstrap :: Maybe FilePath -> IO FilePath
bootstrap fp = do
    putStrLn "- bootstrapping build environment"
    tmpdir <- case fp of
        Just fp' -> return fp'
        Nothing  -> mkdtemp "/tmp/builder."
    doesExist <- doesDirectoryExist tmpdir
    case (doesExist, isNothing fp) of
        (True, True) -> run tmpdir
        (False, False) -> do
            createDirectory tmpdir
            run tmpdir
        _ -> do
            putStrLn "- skipping bootstrap directory exists"
            return tmpdir
    where
        run dir = do
            code <- execute "/usr/bin/pacstrap" ["-d", dir, "base", "base-devel"]
            return $ case code of
                ExitSuccess -> dir
                ExitFailure code -> error $ "Failed to bootstrap, pacstrap exited with: " ++ show code

type NSpawnBind = (String, String)
type NSpawnEnvVar = (String, String)

data NSpawnEnv = NSpawnEnv { binds :: [NSpawnBind]
                           , envs  :: [NSpawnEnvVar]
                           , uid   :: String
                           , shell :: String
                           , wd    :: String
                           } deriving (Show)

nspawn :: FilePath -> [String] -> FilePath -> String -> NSpawnEnv -> IO ExitCode
nspawn path pargs pkgpath repo NSpawnEnv {..} = do
    putStrLn $ "DEBUG: " ++ unwords args
    execute "/usr/bin/systemd-nspawn" args
    return $ ExitFailure 1
        where
            binds' :: [String]
            binds' = concatMap (\(local,container) -> ["--bind", (rpl local) ++ ":" ++ container]) binds
            envs' :: [String]
            envs' = map (\(key,val) -> "--setenv" ++ "=" ++ key ++ "=" ++ val) envs
            rpl :: String -> String
            rpl = (replace "#BPATH#" pkgpath) . (replace "#REPONAME#" repo)
            args = binds' ++ envs' ++ [ "-u", uid
                                      , "-D", wd
                                      , "-x"
                                      , shell, "-c", path ++ " " ++ unwords pargs ]

builderEnv :: FilePath -> NSpawnEnv
builderEnv path = NSpawnEnv { binds = [ ("#BPATH#/repos/#REPONAME#", "/pkgdest")
                                      , ("#BPATH#/buildcache", "/aurdest")
                                      , ("#BPATH#/srcdest", "/srcdest")
                                      , ("#BPATH#/logdest", "/logdest")
                                      , ("/var/cache/pacman/pkg/", "/var/cache/pacman/pkg")
                                      ]
                            , envs = [ ("LANG", "en_AU.UTF-8")
                                     , ("AURDEST", "/aurdest")
                                     , ("PKGDEST", "/pkgdest")
                                     , ("SRCDEST", "/srcdest")
                                     , ("LOGDEST", "/logdest")
                                     , ("MAKEPKG", "--skippgpcheck")
                                     ]
                            , uid = "0"
                            , shell = "/usr/bin/bash"
                            , wd = path
                            }

someFunc :: IO ()
someFunc = do
    path <- bootstrap $ Just "/home/akt/builder"
    putStrLn $ "- using path: " ++ path
    nspawn "/usr/bin/uname" ["-a"] "/home/akt/builder-out" "testing" $ builderEnv path
    putStrLn "someFunc"
