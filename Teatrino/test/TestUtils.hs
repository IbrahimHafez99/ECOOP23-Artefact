-- Shared test helpers: the result type, the expect-* combinators, and
-- the line printer. The actual test cases live in AcceptanceTests and
-- RejectionTests; the example processes live in Examples.
module TestUtils
  ( Result,
    expectGenWellTyped,
    expectSessionWellTyped,
    expectManualWellTyped,
    expectManualError,
    report,
  )
where

import BaseUtils (ErrOr (..))
import Data.List (isInfixOf)
import Parser (parseFile)
import ProcGen (genAllProcs)
import Process (P)
import Session (checkSession, genSession)
import TypeCheck (checkProcs)

-- A test result: a label and whether it passed.
type Result = (String, Bool)

-- The processes generated from a protocol should all type-check.
expectGenWellTyped :: FilePath -> IO Result
expectGenWellTyped protoFile = do
  g <- parseFile protoFile
  let name = "generated well-typed: " ++ protoFile
  case g of
    Ok gg -> case checkProcs gg (genAllProcs gg) of
      Ok _ -> pure (name, True)
      Err _ -> pure (name, False)
    Err _ -> pure (name, False)

-- The session (participants in parallel) built from a protocol should type-check.
expectSessionWellTyped :: FilePath -> IO Result
expectSessionWellTyped protoFile = do
  g <- parseFile protoFile
  let name = "session well-typed: " ++ protoFile
  case g of
    Ok gg -> case checkSession gg (genSession gg) of
      Ok _ -> pure (name, True)
      Err _ -> pure (name, False)
    Err _ -> pure (name, False)

-- Check hand-built processes against a protocol file.
checkManual :: FilePath -> [(String, P)] -> IO (ErrOr [String])
checkManual protoFile ps = do
  g <- parseFile protoFile
  pure (g >>= \gg -> checkProcs gg ps)

-- A hand-built system should be accepted.
expectManualWellTyped :: String -> FilePath -> [(String, P)] -> IO Result
expectManualWellTyped name protoFile ps = do
  r <- checkManual protoFile ps
  pure (name, case r of Ok _ -> True; Err _ -> False)

-- A hand-built system should be rejected, with an error mentioning the given text.
expectManualError :: String -> FilePath -> [(String, P)] -> String -> IO Result
expectManualError name protoFile ps needle = do
  r <- checkManual protoFile ps
  pure (name, case r of Err e -> needle `isInfixOf` e; Ok _ -> False)

-- Print one result line.
report :: Result -> IO ()
report (n, ok) = putStrLn ((if ok then "PASS  " else "FAIL  ") ++ n)
