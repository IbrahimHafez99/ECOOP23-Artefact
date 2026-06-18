-- Builds a process for a role straight from its local type.
-- The local type says what the role should do at each step (send,
-- receive, loop, stop), so we walk it and produce the matching process.
-- Local types come from Projection.hs, the result is checked in TypeCheck.hs.
-- Two simple choices: at a send we take the first label offered, and for
-- payloads we use a default value of the right type.
module ProcGen (genProc, genProcsFromLocals, genAllProcs) where

import BaseUtils (toInt)
import Core (B (..), Choice (..), G, Role, S (..))
import Data.Char (toLower)
import Process (Branch (..), Expr (..), P (..))
import Projection (projAllRoles)

-- Default value for each payload type.
defaultExpr :: B -> Expr
defaultExpr BInt = EInt 0
defaultExpr BReal = EReal 0.0
defaultExpr BString = EStr ""
defaultExpr BBool = EBool True
defaultExpr BUnit = EUnit
-- No literal for user defined types, so use unit.
defaultExpr (BType _ _) = EUnit

-- Pick a readable variable name from a label, so "Quote" becomes "quote".
varName :: String -> String
varName [] = "x"
varName (c : cs) = toLower c : cs

-- Generate the process for one role. The loop list starts empty.
genProc :: S () -> P
genProc = gen []

-- Walk the local type and build the process. The list holds the names of
-- the loops we are currently inside, nearest first, so a numbered jump
-- back can be turned into the right loop name.
gen :: [String] -> S () -> P
-- stop
gen _ SEnd = End
-- send: take the first label with a default payload, then carry on.
gen loops (SSend r _ (c : _)) =
  Send (show r) (show (label c)) (defaultExpr (payload c)) (gen loops (cont c))
  --TODO Error out instead of producing anything.
-- a send with no labels should not happen, but handle it safely.
gen _ (SSend _ _ []) = End
-- receive: make one branch for every label the role might get.
gen loops (SRecv r _ cs) = Recv (show r) (map branchOf cs)
  where
    branchOf c =
      Branch (show (label c)) (varName (show (label c))) (gen loops (cont c))

-- loop: name it by how deep we are (X0, X1 and so on) and remember it.
gen loops (SRec _ t) = Rec name (gen (name : loops) t)
  where
    name = "X" ++ show (length loops)

-- jump back: the number says which loop, so look up its name.
gen loops (SVar n _) = Var (loops !! toInt n)

-- Generate a process for each role, using the local types we are given.
genProcsFromLocals :: [(Role, S ())] -> [(String, P)]
genProcsFromLocals lts = [(show r, genProc t) | (r, t) <- lts]

-- Same but project the global type first.
genAllProcs :: G () -> [(String, P)]
genAllProcs g = genProcsFromLocals (projAllRoles g)
