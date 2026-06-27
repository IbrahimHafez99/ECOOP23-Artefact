-- Tests where the checker should ACCEPT the input (well-typed): generated
-- processes, whole sessions, and correct hand-built systems.
module AcceptanceTests (acceptanceTests) where

import Examples
import TestUtils

acceptanceTests :: [IO Result]
acceptanceTests =
  [ -- processes generated from the protocols should type-check
    expectGenWellTyped "scribble/e_TwoBuyerAll.nuscr",
    expectGenWellTyped "scribble/a_PingPongAll.nuscr",
    -- the whole sessions (roles in parallel) should type-check
    expectSessionWellTyped "scribble/e_TwoBuyerAll.nuscr",
    expectSessionWellTyped "scribble/a_PingPongAll.nuscr",
    -- correct hand-built two-buyer, accepted
    expectManualWellTyped "manual two-buyer well-typed" "scribble/e_TwoBuyerAll.nuscr" twoBuyerManual,
    -- a sender may pick any one offered label: Q choosing quit is accepted
    expectManualWellTyped "manual two-buyer (Q picks quit)" "scribble/e_TwoBuyerAll.nuscr" twoBuyerQuit,
    -- correct hand-built recursive ping-pong, accepted
    expectManualWellTyped "manual ping-pong well-typed" "scribble/a_PingPongAll.nuscr" pingPongManual
  ]
