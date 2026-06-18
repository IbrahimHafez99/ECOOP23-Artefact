{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import BaseUtils ( intercalate', spacer, ErrOr(..) )
import Core ( G )
import Projection ( projAllRoles )
import Parser ( parseFile )
import ProcGen ( genProcsFromLocals )
import Process ( ppProc )
import TypeCheck ( checkProcsFromLocals )
import Effpi ( effpiGIO, Verbosity(Quiet, Loud) )
import PPrinter ( ppG, ppRSList )

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ( takeBaseName )
import System.Console.CmdArgs
    ( Data,
      Typeable,
      (&=),
      cmdArgs,
      explicit,
      help,
      helpArg,
      name,
      program,
      summary,
      typDir,
      typFile,
      verbosityArgs,
      versionArg,
      isLoud,
      Default(def))
import System.Environment (getArgs, withArgs)
import System.Exit ( ExitCode(ExitFailure), exitWith )
import Control.Monad (when, unless)
import GHC.IO.Encoding ( utf8, setLocaleEncoding )

data MyOptions = MyOptions {
    file    :: FilePath,
    outdir  :: FilePath,
    effpi   :: Bool,
    project :: Bool,
    gen     :: Bool
  } deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions {
    file = def &= typFile &= help "Parse a single nuScr file",
    outdir = "scala/" &= typDir &= help "Output directory for generated code",
    effpi = def &= help "Generate Effpi skeleton code",
    project = def &= help "Prints all local types; superseded by --effpi",
    gen = def
      &= help "Generate session calculus processes from the projected local \
              \types, print them, then type-check them"
  }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [explicit, name "version", summary _PROGRAM_INFO]
    &= summary _PROGRAM_INFO
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _PROGRAM_ABOUT :: String
_PROGRAM_NAME = "Teatrino"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Companion generator program for our ECOOP23 submission."

main :: IO ()
main = do
  setLocaleEncoding utf8
  xs <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  opts <- (if null xs then withArgs ["--help"] else id) getOpts
  optionHandler opts

optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..} = do
  -- Check that the input file exists.
  unless (null file) $ do
    t <- doesFileExist file
    if not t
      then putStrLn "File does not exist" >> exitWith (ExitFailure 1)
      else execFile opts

execFile :: MyOptions -> IO ()
execFile _opts@MyOptions{..} = do
  putStrLn ("Input file name: " ++ file)
  when effpi $ putStrLn ("Output Directory: " ++ outdir)
  putStrLn ""
  createDirectoryIfMissing False outdir
  loud <- isLoud
  parseFile file >>= execFile' loud
  where
    execFile' :: Bool -> ErrOr (G ()) -> IO ()
    execFile' _ (Err err) = putStrLn err >> exitWith (ExitFailure 1)
    execFile' loud (Ok g)
      -- Generate Scala code
      | effpi = effpiGIO (if loud then Loud else Quiet) (takeBaseName file) g
      -- Generate session calculus processes from the projected local types,
      -- print them, and then type-check them. Because each process is built.
      | gen = do
        -- Project the global type onto every role ONCE, then share the
        -- result: generate the processes from it, and check them against
        -- it.
        let lts       = projAllRoles g
            generated = genProcsFromLocals lts
        putStrLn ("Generated processes from the projections of "
                  ++ file ++ ":\n")
        mapM_ (\(r, p) ->
                 putStrLn ("proc " ++ r ++ " {\n" ++ ppProc p ++ "\n}\n"))
              generated
        case checkProcsFromLocals lts generated of
          Err err -> putStrLn ("Type error:\n" ++ err)
                     >> exitWith (ExitFailure 1)
          Ok msgs -> do
            putStrLn "Type-checking the generated processes:"
            mapM_ (putStrLn . ("  " ++)) msgs
      -- Print to command line global type and possibly local types
      | otherwise = do
        ppG g
        when (project && loud) $
          putStrLn (intercalate' spacer (map show (projAllRoles g)))
        when (project && not loud) $
          ppRSList (projAllRoles g)
        putStrLn ""

