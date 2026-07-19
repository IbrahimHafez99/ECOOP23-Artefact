{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import BaseUtils ( intercalate', spacer, ErrOr(..) )
import Core ( G )
import Projection ( projAllRoles )
import Parser ( parseFile )
import ProcGen ( genProcsFromLocals )
import Process ( ppProc )
import Session ( genSession, ppSession, checkSession )
import TypeCheck ( checkProcsFromLocals )
import Refactor
    ( Artifacts(..), generateArtifacts
    , refactorRenameRole, refactorRenameLabel
    , LabelSite(..), refactorRenameLabelAtSite )
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
    file          :: FilePath,
    outdir        :: FilePath,
    effpi         :: Bool,
    project       :: Bool,
    gen           :: Bool,
    session       :: Bool,
    refactorRole  :: String,
    refactorLabel :: String,
    relabelSite   :: String
  } deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions {
    file = def &= typFile &= help "Parse a single nuScr file",
    outdir = "scala/" &= typDir &= help "Output directory for generated code",
    effpi = def &= help "Generate Effpi skeleton code",
    project = def &= help "Prints all local types; superseded by --effpi",
    gen = def
      &= help "Generate session calculus processes from the projected local \
              \types, print them, then type-check them",
    session = def
      &= help "Build the session (participants in parallel) from the protocol, \
              \print it, then type-check every participant",
    refactorRole = def
      &= help "Rename a role across the protocol, local types and generated \
              \processes, then re-check. Format: --refactorrole=Old:New",
    refactorLabel = def
      &= help "Rename a label across the protocol, local types and generated \
              \processes, then re-check. Format: --refactorlabel=Old:New",
    relabelSite = def
      &= help "Rename a label at ONE message only (split): rename the message \
              \Old between the two given roles to New, leaving other messages \
              \with that label unchanged. Format: \
              \--relabelsite=Sender:Receiver:Old:New"
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
      -- Build the session (participants in parallel) from the protocol,
      -- print it, then type-check every participant.
      | session = do
        let sess = genSession g
        putStrLn ("Session for " ++ file ++ ":\n")
        putStrLn (ppSession sess)
        putStrLn ""
        case checkSession g sess of
          Err err -> putStrLn ("Type error:\n" ++ err)
                     >> exitWith (ExitFailure 1)
          Ok msgs -> do
            putStrLn "Type-checking the session participants:"
            mapM_ (putStrLn . ("  " ++)) msgs
      -- Rename a role in place across the generated artifacts, then re-check.
      | not (null refactorRole) =
        case splitPair refactorRole of
          Nothing -> badPair "refactorrole"
          Just (old, new) ->
            runRefactor (refactorRenameRole old new) (generateArtifacts g)
      -- Rename a label in place across the generated artifacts, then re-check.
      | not (null refactorLabel) =
        case splitPair refactorLabel of
          Nothing -> badPair "refactorlabel"
          Just (old, new) ->
            runRefactor (refactorRenameLabel old new) (generateArtifacts g)
      -- Rename a label at one message only (split), then re-check.
      | not (null relabelSite) =
        case splitSite relabelSite of
          Nothing ->
            putStrLn "Use --relabelsite=Sender:Receiver:Old:New"
              >> exitWith (ExitFailure 1)
          Just (sndr, rcvr, old, new) ->
            runRefactor
              (refactorRenameLabelAtSite (LabelSite sndr rcvr old) new)
              (generateArtifacts g)
      -- Print to command line global type and possibly local types
      | otherwise = do
        ppG g
        when (project && loud) $
          putStrLn (intercalate' spacer (map show (projAllRoles g)))
        when (project && not loud) $
          ppRSList (projAllRoles g)
        putStrLn ""

    -- Split an "Old:New" argument into its two halves.
    splitPair :: String -> Maybe (String, String)
    splitPair s = case break (== ':') s of
      (a, ':' : b) | not (null a) && not (null b) -> Just (a, b)
      _ -> Nothing

    -- Split on every ':' into pieces.
    wordsOn :: Char -> String -> [String]
    wordsOn ch s = case break (== ch) s of
      (a, _ : rest) -> a : wordsOn ch rest
      (a, "") -> [a]

    -- Split a "Sender:Receiver:Old:New" argument into its four parts.
    splitSite :: String -> Maybe (String, String, String, String)
    splitSite s = case wordsOn ':' s of
      [a, b, c, d] | all (not . null) [a, b, c, d] -> Just (a, b, c, d)
      _ -> Nothing

    badPair :: String -> IO ()
    badPair flagName =
      putStrLn ("Use --" ++ flagName ++ "=Old:New")
        >> exitWith (ExitFailure 1)

    -- Print the three artifacts (global type, local types, processes).
    printArtifacts :: Artifacts -> IO ()
    printArtifacts arts = do
      putStrLn "global type:"
      ppG (aGlobal arts)
      putStrLn "\nlocal types:"
      ppRSList (aLocals arts)
      putStrLn "processes:"
      mapM_ (\(r, p) ->
               putStrLn ("proc " ++ r ++ " {\n" ++ ppProc p ++ "\n}\n"))
            (aProcs arts)

    -- Show the generated artifacts, apply the refactoring IN PLACE (the
    -- refactoring edits each artifact directly, it does not project again),
    -- then show the result. The refactoring already re-checked the edited
    -- local types against the edited processes, so reaching Ok means it passed.
    runRefactor :: (Artifacts -> ErrOr Artifacts) -> Artifacts -> IO ()
    runRefactor refac arts = do
      putStrLn "Generated artifacts (before refactoring):\n"
      printArtifacts arts
      case refac arts of
        Err e -> putStrLn ("Refactoring rejected:\n  " ++ e)
                 >> exitWith (ExitFailure 1)
        Ok arts' -> do
          putStrLn "Refactored artifacts (renamed in place, no re-projection):\n"
          printArtifacts arts'
          case checkProcsFromLocals (aLocals arts') (aProcs arts') of
            Err err -> putStrLn ("Type error after refactoring:\n" ++ err)
                       >> exitWith (ExitFailure 1)
            Ok msgs -> do
              putStrLn "Re-checking the edited local types against the edited processes:"
              mapM_ (putStrLn . ("  " ++)) msgs

