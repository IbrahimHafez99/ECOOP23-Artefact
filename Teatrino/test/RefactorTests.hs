
module RefactorTests (refactorTests) where

import Examples (twoBuyerManual)
import Refactor
  ( Artifacts (..),
    LabelSite (..),
    refactorRenameLabel,
    refactorRenameLabelAtSite,
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
      "already a role",
    -- split: rename Quote only on the R -> P message; the R -> Q Quote stays
    expectRefactorOk
      "refactor: rename label at site R->P Quote -> Price (accepted)"
      proto
      twoBuyerManual
      (refactorRenameLabelAtSite (LabelSite "R" "P" "Quote") "Price"),
    -- site precondition: new clashes with a sibling of that choice (Q->R {Ok,Quit})
    expectRefactorRejected
      "refactor: rename label at site onto a sibling (rejected)"
      proto
      twoBuyerManual
      (refactorRenameLabelAtSite (LabelSite "Q" "R" "Ok") "Quit")
      "sibling",
    -- site precondition: the message does not exist
    expectRefactorRejected
      "refactor: rename label at a site that does not exist (rejected)"
      proto
      twoBuyerManual
      (refactorRenameLabelAtSite (LabelSite "R" "P" "Banana") "Price")
      "no message"
  ]
