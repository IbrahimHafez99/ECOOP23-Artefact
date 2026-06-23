-- Type checking processes against the protocol.
module TypeCheck (checkProcs, checkProcsFromLocals, checkProc, typeOfExpr) where

import BaseUtils (ErrOr (..), intercalate', toInt)
import Core (B (..), Choice (..), G, Role, S (..))
import Data.List (find)
import Process (Branch (..), Expr (..), P (..))
import Projection (projAllRoles)

-- Variables that are in scope (bound by receives), ex notebook: [("quote", Double), ("addr", String)]..
-- we need it Because later, when the process sends a variable, we have to know its type , and the only way to know is to remember it from when it was received. The Env is that memory.
type Env = [(String, B)]

-- Work out the sort of a payload expression.
typeOfExpr :: Env -> Expr -> ErrOr B
typeOfExpr env (EVar x) = case lookup x env of
  Nothing -> Err ("unbound variable: " ++ x)
  Just b -> Ok b
typeOfExpr _ (EInt _) = Ok BInt
typeOfExpr _ (EReal _) = Ok BReal
typeOfExpr _ (EStr _) = Ok BString
typeOfExpr _ (EBool _) = Ok BBool
typeOfExpr _ EUnit = Ok BUnit

-- Check one process against one local type.
checkProc :: S () -> P -> ErrOr ()
checkProc t p = go [] [] 0 p t

-- The walk itself. Arguments:
--   env   variables in scope and their sorts
--   recs  for each open process loop, the depth at which it was opened
--   d     how many type-side recs we are currently inside
go :: Env -> [(String, Int)] -> Int -> P -> S () -> ErrOr ()
-- A loop in the process must sit exactly on a loop in the type.
-- Remember at which depth this loop was opened, then check the body.
go env recs d (Rec x p) (SRec _ t) = go env ((x, d) : recs) (d + 1) p t
-- continue X must sit where the type has its recursion variable. at depth d - 1 - n. That has to be the loop X was opened at.
go _ recs d (Var x) (SVar n _) = case lookup x recs of
  Nothing -> Err ("unknown recursion variable: " ++ x)
  Just dx
    | dx == d - 1 - toInt n -> Ok ()
    | otherwise ->
        Err
          ( "'continue "
              ++ x
              ++ "' jumps to a different loop "
              ++ "than the protocol does here"
          )
-- Both branches of an if must follow the same local type.
go env recs d (If e p1 p2) t = do
  te <- typeOfExpr env e
  if te /= BBool
    then
      Err
        ( "condition must be Bool, but "
            ++ show e
            ++ " has sort "
            ++ show te
        )
    else go env recs d p1 t >> go env recs d p2 t

-- A finished process matches end.
go _ _ _ End SEnd = Ok ()
-- Send: right partner, offered label, right payload sort, then carry on
-- with that label's continuation.
go env recs d (Send q l e k) (SSend r _ ks)
  | q /= show r =
      Err
        ( "process sends to "
            ++ q
            ++ ", but the protocol expects a send to "
            ++ show r
        )
  | otherwise = case find (\c -> show (label c) == l) ks of
      Nothing ->
        Err
          ( "label "
              ++ l
              ++ " is not one of the options here; "
              ++ "expected one of "
              ++ show (map label ks)
          )
      Just c -> do
        te <- typeOfExpr env e
        if te /= payload c
          then
            Err
              ( "payload "
                  ++ show e
                  ++ " of "
                  ++ q
                  ++ "!"
                  ++ l
                  ++ " has sort "
                  ++ show te
                  ++ ", expected "
                  ++ show (payload c)
              )
          else go env recs d k (cont c)

-- Receive: right partner, no impossible branches, and every label the
-- type can deliver must be handled. Inside a branch the bound variable
-- gets the payload's sort.
go env recs d (Recv q bs) (SRecv r _ ks)
  | q /= show r =
      Err
        ( "process receives from "
            ++ q
            ++ ", but the protocol expects a receive from "
            ++ show r
        )
  | not (null extras) =
      Err
        ( "process has branches for "
            ++ show extras
            ++ " which can never arrive from "
            ++ q
            ++ "; the options here are "
            ++ show (map label ks)
        )
  | otherwise = checkAllBranches ks
  where
    extras = [bLabel b | b <- bs, bLabel b `notElem` map (show . label) ks]

    checkAllBranches [] = Ok ()
    checkAllBranches (c : cs) =
      case find (\b -> bLabel b == show (label c)) bs of
        Nothing ->
          Err
            ( "process does not handle label "
                ++ show (label c)
                ++ " from "
                ++ q
            )
        Just b -> do
          go ((bVar b, payload c) : env) recs d (bCont b) (cont c)
          checkAllBranches cs

-- Anything else is a mismatch (ex sending where the type receives,
-- or stopping while the protocol carries on).
go _ _ _ p t =
  Err
    ( "process does not follow the local type here.\n  process: "
        ++ show p
        ++ "\n  local type: "
        ++ show t
        ++ "\n  (note: loops in the process must appear exactly where "
        ++ "the protocol loops)"
    )

checkProcsFromLocals :: [(Role, S ())] -> [(String, P)] -> ErrOr [String]
-- Define the function using two inputs: local types lts and processes ps.
checkProcsFromLocals lts ps =
  -- Check every process with f, if successful, add skipped-role messages.
  fmap (++ skipped) (mapM f ps)
  where
    -- f checks one process pair: role name rname and process p which is (String, P).
    f (rname, p) =
      -- Search lts for the local type belonging to this role name.
      case find (\(r, _) -> show r == rname) lts of
        -- If no matching role is found, return an error.
        Nothing ->
          Err
            -- Start the error message.
            ( "role "
                -- Add the role name that was not found.
                ++ rname
                -- Explain the problem.
                ++ " is not part of this protocol; "
                -- Start listing valid participants.
                ++ "the participants are "
                -- Join all protocol role names with commas.
                ++ intercalate' ", " (map (show . fst) lts)
            )
        -- If a matching role is found, ignore the role and keep its local type t.
        Just (_, t) ->
          -- Check process p against local type t.
          case checkProc t p of
            -- If the process type-checks, return a success message.
            Ok () -> Ok (rname ++ ": well-typed")
            -- If checking fails, attach the role name to the error.
            Err e -> Err ("role " ++ rname ++ ":\n" ++ e)

    -- Build messages for protocol roles that have no process.
    skipped =
      -- This is a list comprehension: it creates a list of strings.
      [ show r ++ ": no process given (skipped)"
        | -- Go through every role/local-type pair in lts.
          (r, _) <- lts,
          -- Keep only roles whose name is not in the process-name list.
          show r `notElem` map fst ps
      ]

-- Project the global type onto every role, then check. Used by the
-- hand-written process path and by the test suite.
checkProcs :: G () -> [(String, P)] -> ErrOr [String]
checkProcs g = checkProcsFromLocals (projAllRoles g)
