
module Session
  ( M (..),
    mkSession,
    genSession,
    participants,
    ppSession,
    checkSession,
  )
where

import BaseUtils (ErrOr)
import Core (G, Role)
import ProcGen (genProc)
import Process (P, ppProc)
import Projection (projAllRoles)
import TypeCheck (checkProcsFromLocals)

-- A session: participants running in parallel.
data M
  = Part Role P -- p <| P : role p running process P (one participant running one process)
  | Par M M -- M | M : parallel composition (two session running togather)
  deriving (Show)

-- takes a list of participants and turns it into one session M. A session has at least one
-- participant (there is no empty session), so the empty list is not used.
mkSession :: [(Role, P)] -> M
mkSession [] = error "mkSession: a session needs at least one participant"
-- if the list has exactly one participant, it makes a single Part
mkSession [(r, p)] = Part r p
-- 
mkSession ((r, p) : ps) = Par (Part r p) (mkSession ps)

-- Build a session from a protocol: project onto every role, generate one
-- process per role, then put the participants in parallel.
genSession :: G () -> M
genSession g = mkSession [(r, genProc t) | (r, t) <- projAllRoles g]

-- Flatten a session back to its list of participants.
participants :: M -> [(Role, P)]
participants (Part r p) = [(r, p)]
participants (Par m1 m2) = participants m1 ++ participants m2

-- Print a session: each participant, joined by the parallel bar.
ppSession :: M -> String
ppSession (Part r p) = show r ++ " <| {\n" ++ ppProc p ++ "\n}"
ppSession (Par m1 m2) = ppSession m1 ++ "\n|\n" ++ ppSession m2

-- Type-check a session: check every participant's process against its
-- role's local type.
checkSession :: G () -> M -> ErrOr [String]
checkSession g m =
  checkProcsFromLocals (projAllRoles g) [(show r, p) | (r, p) <- participants m]
