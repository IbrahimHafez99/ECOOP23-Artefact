
module RefactorTests (refactorTests) where

import Examples (twoBuyerManual)
import Refactor
  ( Artifacts (..),
    refactorRenameLabel,
    refactorRenameRole,
    renameRoleInSystem,
  )
import TestUtils
import TypeCheck (checkProcsFromLocals)

proto :: FilePath
proto = "scribble/e_TwoBuyerAll.nuscr"

refactorTests :: [IO Result]
refactorTests =
  [ -- rename a role in place across all artifacts: still well-typed
    expectRefactorOk
      "refactor: rename role R -> Seller (accepted)"
      proto
      twoBuyerManual
      (refactorRenameRole "R" "Seller"),
    -- rename a label in place across all artifacts: still well-typed
    expectRefactorOk
      "refactor: rename label Quote -> Price (accepted)"
      proto
      twoBuyerManual
      (refactorRenameLabel "Quote" "Price"),
    -- edit only the processes, leave the local types unchanged: rejected
    expectRefactorRejected
      "refactor: rename role in processes only (rejected)"
      proto
      twoBuyerManual
      ( \arts ->
          checkProcsFromLocals
            (aLocals arts)
            (renameRoleInSystem "R" "Seller" (aProcs arts))
      )
      "expects a send to",
    -- precondition: the old role does not exist
    expectRefactorRejected
      "refactor: rename a role that does not exist (rejected)"
      proto
      twoBuyerManual
      (refactorRenameRole "Zzz" "Seller")
      "is not a role",
    -- precondition: the new name is already a role (would merge two roles)
    expectRefactorRejected
      "refactor: rename role onto an existing role (rejected)"
      proto
      twoBuyerManual
      (refactorRenameRole "P" "Q")
      "already a role"
  ]
