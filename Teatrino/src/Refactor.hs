-- Refactorings that rename a role or a label.

module Refactor
  ( -- the in-memory snapshot of the three generated artifacts
    Artifacts (..),
    generateArtifacts,
    withProcs,
    -- rename in one artifact
    renameRoleInP,
    renameRoleInS,
    renameRoleInG,
    renameRoleRole,
    renameLabelInP,
    renameLabelInS,
    renameLabelInG,
    -- rename across a list of processes
    renameRoleInSystem,
    renameLabelInSystem,
    -- rename across the whole snapshot, in place
    renameRoleEverywhere,
    renameLabelEverywhere,
    -- the names already in a protocol (used by the preconditions)
    rolesInG,
    labelsInG,
    -- preconditions
    preRenameRole,
    preRenameLabel,
    -- refactor + verify (in place, no re-projection)
    refactorRenameRole,
    refactorRenameLabel,
  )
where

import BaseUtils (ErrOr (..))
import Core (Choice (..), G (..), Label (..), Role (..), S (..))
import Data.List (nub)
import ProcGen (genProcsFromLocals)
import Process (Branch (..), P (..))
import Projection (projAllRoles)
import TypeCheck (checkProcsFromLocals)

-- The three generated artifacts, held together as one snapshot and refactorings edit this.

data Artifacts = Artifacts
  { aGlobal :: G (),
    aLocals :: [(Role, S ())],
    aProcs :: [(String, P)]
  }

-- The one-time generation step: project once to get the local types, then generate the processes from them through refactoring we never repeats this
generateArtifacts :: G () -> Artifacts
generateArtifacts g =
  let ls = projAllRoles g
   in Artifacts g ls (genProcsFromLocals ls)

-- Same snapshot but with hand-written processes provided instead of generated ones. Used by the tests so we refactor existing processes.
withProcs :: G () -> [(String, P)] -> Artifacts
withProcs g procs = Artifacts g (projAllRoles g) procs

-- Rename a role

-- In a process: change the name in every send and receive.
renameRoleInP :: String -> String -> P -> P
renameRoleInP old new = go
  where
    rn q = if q == old then new else q
    go (Send q l e k) = Send (rn q) l e (go k)
    go (Recv q bs) = Recv (rn q) (map goB bs)
    go (If e p1 p2) = If e (go p1) (go p2)
    go (Rec x p) = Rec x (go p)
    go (Var x) = Var x
    go End = End
    goB (Branch l x k) = Branch l x (go k)

-- A single role value: keep the id, swap only the name.
renameRoleRole :: String -> String -> Role -> Role
renameRoleRole old new r@(MkRole i nm rel) =
  if nm == old then MkRole i new rel else r

-- In a local type: rename the partner role of each send/receive.
-- A local type S a. It returns an updated local type of the same shape. The type variable a means the local type may contain annotations of any type. The function preserves those annotations.
renameRoleInS :: String -> String -> S a -> S a
renameRoleInS old new = go
  where
    go (SSend r a cs) = SSend (renameRoleRole old new r) a (map goC cs)
    go (SRecv r a cs) = SRecv (renameRoleRole old new r) a (map goC cs)
    go (SRec a t) = SRec a (go t)
    go (SVar n a) = SVar n a
    go SEnd = SEnd
    -- rename inside this branch's continuation, leave the branch's label and payload alone / oldRecord { field = newValue }
    goC c = c {cont = go (cont c)}

-- In the global type: rename the name carried by every matching role.
renameRoleInG :: String -> String -> G a -> G a
renameRoleInG old new = go
  where
    rn = renameRoleRole old new
    go (GComm p q a cs) = GComm (rn p) (rn q) a (map goC cs)
    go (GRec a g) = GRec a (go g)
    go (GVar n a) = GVar n a
    go GEnd = GEnd
    goC c = c {cont = go (cont c)}

-- Rename a label

renameLabelInP :: String -> String -> P -> P
renameLabelInP old new = go
  where
    rn l = if l == old then new else l
    go (Send q l e k) = Send q (rn l) e (go k)
    go (Recv q bs) = Recv q (map goB bs)
    go (If e p1 p2) = If e (go p1) (go p2)
    go (Rec x p) = Rec x (go p)
    go (Var x) = Var x
    go End = End
    goB (Branch l x k) = Branch (rn l) x (go k)

renameLabelInS :: String -> String -> S a -> S a
renameLabelInS old new = go
  where
    go (SSend r a cs) = SSend r a (map goC cs)
    go (SRecv r a cs) = SRecv r a (map goC cs)
    go (SRec a t) = SRec a (go t)
    go (SVar n a) = SVar n a
    go SEnd = SEnd
    goC c = c {label = rnL old new (label c), cont = go (cont c)}

renameLabelInG :: String -> String -> G a -> G a
renameLabelInG old new = go
  where
    go (GComm p q a cs) = GComm p q a (map goC cs)
    go (GRec a g) = GRec a (go g)
    go (GVar n a) = GVar n a
    go GEnd = GEnd
    goC c = c {label = rnL old new (label c), cont = go (cont c)}

-- Shared helper: rename one label value, leaving crash alone.
rnL :: String -> String -> Label -> Label
rnL old new (MkLabel i nm) = MkLabel i (if nm == old then new else nm)
rnL _ _ CrashLab = CrashLab

-- Whole-system helpers (a system is a list of named processes)

-- Rename a role in every process and in the role keys.
renameRoleInSystem :: String -> String -> [(String, P)] -> [(String, P)]
renameRoleInSystem old new sys =
  [(if r == old then new else r, renameRoleInP old new p) | (r, p) <- sys]

-- Rename a label in every process.
renameLabelInSystem :: String -> String -> [(String, P)] -> [(String, P)]
renameLabelInSystem old new sys =
  [(r, renameLabelInP old new p) | (r, p) <- sys]

-- Rename across the whole snapshot, editing each artifact in place.
-- Note there is no projAllRoles here: the local types are renamed
-- directly, not re-derived from the renamed global type.

renameRoleEverywhere :: String -> String -> Artifacts -> Artifacts
renameRoleEverywhere old new (Artifacts g ls ps) =
  Artifacts
    (renameRoleInG old new g)
    [(renameRoleRole old new r, renameRoleInS old new s) | (r, s) <- ls]
    (renameRoleInSystem old new ps)

renameLabelEverywhere :: String -> String -> Artifacts -> Artifacts
renameLabelEverywhere old new (Artifacts g ls ps) =
  Artifacts
    (renameLabelInG old new g)
    [(r, renameLabelInS old new s) | (r, s) <- ls]
    (renameLabelInSystem old new ps)

-- The names a protocol already uses (for the preconditions)

rolesInG :: G a -> [String]
rolesInG = nub . go
  where
    go (GComm p q _ cs) = show p : show q : concatMap (go . cont) cs
    go (GRec _ g) = go g
    go (GVar _ _) = []
    go GEnd = []

labelsInG :: G a -> [String]
labelsInG = nub . go
  where
    go (GComm _ _ _ cs) = concatMap (\c -> show (label c) : go (cont c)) cs
    go (GRec _ g) = go g
    go (GVar _ _) = []
    go GEnd = []

-- Preconditions

-- Rename a role old -> new is allowed when:
--   old and new are different,
--   old is a role in the protocol,
--   new is not already a role
preRenameRole :: String -> String -> G () -> ErrOr ()
preRenameRole old new g
  | old == new =
      Err "rename role: the old and new names are the same"
  | old `notElem` rs =
      Err ("rename role: '" ++ old ++ "' is not a role in this protocol")
  | new `elem` rs =
      Err
        ( "rename role: '"
            ++ new
            ++ "' is already a role"
        )
  | otherwise = Ok ()
  where
    rs = rolesInG g

-- Rename a label old -> new is allowed when:
--   old and new are different,
--   old is a label in the protocol,
--   new does not already label a sibling branch in the same choice (two branches of one choice must have distinct labels).
preRenameLabel :: String -> String -> G () -> ErrOr ()
preRenameLabel old new g
  | old == new =
      Err "rename label: the old and new names are the same"
  | old `notElem` labelsInG g =
      Err ("rename label: '" ++ old ++ "' is not a label in this protocol")
  | clash g =
      Err
        ( "rename label: '"
            ++ new
            ++ "' already labels another branch of the same choice"
        )
  | otherwise = Ok ()
  where
    clash (GComm _ _ _ cs) =
      let names = map (show . label) cs
       in (old `elem` names && new `elem` names) || any (clash . cont) cs
    clash (GRec _ g') = clash g'
    clash (GVar _ _) = False
    clash GEnd = False

-- Refactor and verify, in place.
--   1. check the precondition on the global type,
--   2. rename all three artifacts directly (no projection),
--   3. re-check the edited local types against the edited processes.
-- checkProcsFromLocals uses the local types we hand it, so this verifies the artifacts we actually edited, not newly projected ones.

refactorRenameRole :: String -> String -> Artifacts -> ErrOr Artifacts
refactorRenameRole old new arts = do
  preRenameRole old new (aGlobal arts)
  let arts' = renameRoleEverywhere old new arts
  _ <- checkProcsFromLocals (aLocals arts') (aProcs arts')
  pure arts'

refactorRenameLabel :: String -> String -> Artifacts -> ErrOr Artifacts
refactorRenameLabel old new arts = do
  preRenameLabel old new (aGlobal arts)
  let arts' = renameLabelEverywhere old new arts
  _ <- checkProcsFromLocals (aLocals arts') (aProcs arts')
  pure arts'
