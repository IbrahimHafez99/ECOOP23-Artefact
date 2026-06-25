
module Main (main) where

import BaseUtils (ErrOr (..))
import Parser (parseFile)
import Process (Branch (..), Expr (..), P (..))
import ProcGen (genAllProcs)
import Session (checkSession, genSession)
import TypeCheck (checkProcs)

import Data.List (isInfixOf)
import System.Exit (exitFailure, exitSuccess)

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

-- The session (participants in parallel) built from a protocol should
-- type-check.
expectSessionWellTyped :: FilePath -> IO Result
expectSessionWellTyped protoFile = do
  g <- parseFile protoFile
  let name = "session well-typed: " ++ protoFile
  case g of
    Ok gg -> case checkSession gg (genSession gg) of
      Ok _ -> pure (name, True)
      Err _ -> pure (name, False)
    Err _ -> pure (name, False)

-- Two-Buyer written out by hand with the constructors, no parser involved.
manualP :: P
manualP =
  Send "R" "Title" (EStr "War and Peace")
    (Recv "R" [Branch "Quote" "x"
      (Send "Q" "Split" (EVar "x") End)])

manualQ :: P
manualQ =
  Recv "R" [Branch "Quote" "y"
    (Recv "P" [Branch "Split" "z"
      (If (EBool True)
        (Send "R" "Ok" EUnit
          (Send "R" "Addr" (EStr "13 Sandy Lane")
            (Recv "R" [Branch "Date" "d" End])))
        (Send "R" "Quit" EUnit End))])]

manualR :: P
manualR =
  Recv "P" [Branch "Title" "t"
    (Send "P" "Quote" (EReal 15.0)
      (Send "Q" "Quote" (EReal 15.0)
        (Recv "Q"
          [ Branch "Ok" "u"
              (Recv "Q" [Branch "Addr" "a"
                (Send "Q" "Date" (EStr "2026-06-12") End)]),
            Branch "Quit" "u" End ])))]

twoBuyerManual :: [(String, P)]
twoBuyerManual = [("P", manualP), ("Q", manualQ), ("R", manualR)]

-- Same as manualP but it sends an Int where the protocol wants a Double.
manualPbad :: P
manualPbad =
  Send "R" "Title" (EStr "War and Peace")
    (Recv "R" [Branch "Quote" "x"
      (Send "Q" "Split" (EInt 42) End)])

twoBuyerManualBad :: [(String, P)]
twoBuyerManualBad = [("P", manualPbad), ("Q", manualQ), ("R", manualR)]

-- A few more P's that should all be rejected, each breaking a different rule.

-- sends the first message to Q instead of R
badPartnerP :: P
badPartnerP =
  Send "Q" "Title" (EStr "War and Peace")
    (Recv "R" [Branch "Quote" "x"
      (Send "Q" "Split" (EVar "x") End)])

-- uses a label the protocol never offers
badLabelP :: P
badLabelP =
  Send "R" "Banana" (EStr "War and Peace")
    (Recv "R" [Branch "Quote" "x"
      (Send "Q" "Split" (EVar "x") End)])

-- stops after the first message instead of carrying on
earlyEndP :: P
earlyEndP = Send "R" "Title" (EStr "War and Peace") End

twoBuyerBadPartner, twoBuyerBadLabel, twoBuyerEarlyEnd :: [(String, P)]
twoBuyerBadPartner = [("P", badPartnerP), ("Q", manualQ), ("R", manualR)]
twoBuyerBadLabel = [("P", badLabelP), ("Q", manualQ), ("R", manualR)]
twoBuyerEarlyEnd = [("P", earlyEndP), ("Q", manualQ), ("R", manualR)]

-- Ping-Pong written out by hand. This one has a loop (rec / continue).
manualPingP :: P
manualPingP =
  Rec "X0"
    (Send "Q" "Ping" EUnit
      (Recv "Q" [Branch "Pong" "p" (Var "X0")]))

manualPingQ :: P
manualPingQ =
  Rec "X0"
    (Recv "P" [Branch "Ping" "p"
      (Send "P" "Pong" EUnit (Var "X0"))])

pingPongManual :: [(String, P)]
pingPongManual = [("P", manualPingP), ("Q", manualPingQ)]

-- Check hand-built processes against a protocol file.
checkManual :: FilePath -> [(String, P)] -> IO (ErrOr [String])
checkManual protoFile ps = do
  g <- parseFile protoFile
  pure (g >>= \gg -> checkProcs gg ps)

expectManualWellTyped :: String -> FilePath -> [(String, P)] -> IO Result
expectManualWellTyped name protoFile ps = do
  r <- checkManual protoFile ps
  pure (name, case r of Ok _ -> True; Err _ -> False)

expectManualError :: String -> FilePath -> [(String, P)] -> String -> IO Result
expectManualError name protoFile ps needle = do
  r <- checkManual protoFile ps
  pure (name, case r of Err e -> needle `isInfixOf` e; Ok _ -> False)

main :: IO ()
main = do
  results <-
    sequence
      [ -- processes generated from the two-buyer protocol should type-check
        expectGenWellTyped "scribble/e_TwoBuyerAll.nuscr",
        -- processes generated from the ping-pong protocol should type-check
        expectGenWellTyped "scribble/a_PingPongAll.nuscr",
        -- the whole two-buyer session (roles in parallel) should type-check
        expectSessionWellTyped "scribble/e_TwoBuyerAll.nuscr",
        -- the whole ping-pong session should type-check
        expectSessionWellTyped "scribble/a_PingPongAll.nuscr",
        -- two-buyer built by hand with the constructors, should be accepted
        expectManualWellTyped "manual two-buyer well-typed" "scribble/e_TwoBuyerAll.nuscr" twoBuyerManual,
        -- hand-built P sends an Int instead of a Double, should be rejected
        expectManualError "manual two-buyer rejected (Int)" "scribble/e_TwoBuyerAll.nuscr" twoBuyerManualBad "Int",
        -- hand-built P sends to the wrong role, should be rejected
        expectManualError "manual rejected (wrong partner)" "scribble/e_TwoBuyerAll.nuscr" twoBuyerBadPartner "expects a send to",
        -- hand-built P uses a label the protocol never offers, should be rejected
        expectManualError "manual rejected (wrong label)" "scribble/e_TwoBuyerAll.nuscr" twoBuyerBadLabel "not one of the options",
        -- hand-built P stops too early, should be rejected
        expectManualError "manual rejected (ends too early)" "scribble/e_TwoBuyerAll.nuscr" twoBuyerEarlyEnd "does not follow the local type",
        -- recursive ping-pong built by hand, should be accepted
        expectManualWellTyped "manual ping-pong well-typed" "scribble/a_PingPongAll.nuscr" pingPongManual
      ]
  mapM_ report results
  putStrLn ""
  let failed = length (filter (not . snd) results)
  if failed == 0
    then putStrLn (show (length results) ++ " tests passed.") >> exitSuccess
    else
      putStrLn (show failed ++ " of " ++ show (length results) ++ " tests failed.")
        >> exitFailure
  where
    report (n, ok) = putStrLn ((if ok then "PASS  " else "FAIL  ") ++ n)
