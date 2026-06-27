-- Tests where the checker should REJECT the input. Each one breaks a
-- different rule, and we also check the error message mentions the right
-- thing so a test can't pass for the wrong reason.
module RejectionTests (rejectionTests) where

import Examples
import TestUtils

proto :: FilePath
proto = "scribble/e_TwoBuyerAll.nuscr"

pingProto :: FilePath
pingProto = "scribble/a_PingPongAll.nuscr"

rejectionTests :: [IO Result]
rejectionTests =
  [ -- payload of the wrong type (Int where Double is required)
    expectManualError "rejected (payload type)" proto twoBuyerManualBad "Int",
    -- sends to the wrong role
    expectManualError "rejected (send wrong partner)" proto twoBuyerBadPartner "expects a send to",
    -- sends a label the protocol does not offer
    expectManualError "rejected (send wrong label)" proto twoBuyerBadLabel "not one of the options",
    -- stops before the protocol is finished
    expectManualError "rejected (ends too early)" proto twoBuyerEarlyEnd "does not follow the local type",
    -- receives from the wrong role
    expectManualError "rejected (receive wrong partner)" proto twoBuyerRecvPartner "expects a receive from",
    -- sends an extra message after the protocol has ended
    expectManualError "rejected (extra message)" proto twoBuyerExtraSend "does not follow the local type",
    -- receives where the protocol expects a send
    expectManualError "rejected (receive where send expected)" proto twoBuyerRecvWhereSend "does not follow the local type",
    -- sends a variable that was never received
    expectManualError "rejected (unbound variable)" proto twoBuyerUnbound "unbound variable",
    -- uses a non-boolean if condition
    expectManualError "rejected (if not Bool)" proto twoBuyerBadIf "condition must be Bool",
    -- misses a branch the protocol can send
    expectManualError "rejected (missing branch)" proto twoBuyerRecvMissing "does not handle label",
    -- handles a branch that can never arrive
    expectManualError "rejected (impossible branch)" proto twoBuyerRecvExtra "can never arrive",
    -- a process is given for a role not in the protocol
    expectManualError "rejected (unknown role)" proto twoBuyerExtraRole "is not part of this protocol",
    -- continues to a loop that was never opened
    expectManualError "rejected (unknown loop var)" pingProto pingPongBadVar "unknown recursion variable"
  ]
