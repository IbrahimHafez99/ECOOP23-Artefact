module Examples where

import Process (Branch (..), Expr (..), P (..))

-- Two-Buyer

-- The correct implementation, one process per role.
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

-- Q just sends quit with no if: a sender may pick any one offered label.
quitVariantQ :: P
quitVariantQ =
  Recv "R" [Branch "Quote" "y"
    (Recv "P" [Branch "Split" "z"
      (Send "R" "Quit" EUnit End)])]

twoBuyerQuit :: [(String, P)]
twoBuyerQuit = [("P", manualP), ("Q", quitVariantQ), ("R", manualR)]

-- --- the broken variants, one per checker rule ---

-- sends an Int where the protocol wants a Double
manualPbad :: P
manualPbad =
  Send "R" "Title" (EStr "War and Peace")
    (Recv "R" [Branch "Quote" "x"
      (Send "Q" "Split" (EInt 42) End)])

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

-- receives the quote from Q instead of R
recvWrongPartnerP :: P
recvWrongPartnerP =
  Send "R" "Title" (EStr "War and Peace")
    (Recv "Q" [Branch "Quote" "x"
      (Send "Q" "Split" (EVar "x") End)])

-- sends an extra message after the protocol has ended
extraSendP :: P
extraSendP =
  Send "R" "Title" (EStr "War and Peace")
    (Recv "R" [Branch "Quote" "x"
      (Send "Q" "Split" (EVar "x")
        (Send "R" "Extra" EUnit End))])

-- does a receive where the protocol expects a send
recvWhereSendP :: P
recvWhereSendP = Recv "R" [Branch "Quote" "x" End]

-- sends a variable that was never received
unboundP :: P
unboundP =
  Send "R" "Title" (EStr "War and Peace")
    (Recv "R" [Branch "Quote" "x"
      (Send "Q" "Split" (EVar "zzz") End)])

-- the if condition is a number instead of a boolean
badIfQ :: P
badIfQ =
  Recv "R" [Branch "Quote" "y"
    (Recv "P" [Branch "Split" "z"
      (If (EInt 5)
        (Send "R" "Ok" EUnit
          (Send "R" "Addr" (EStr "13 Sandy Lane")
            (Recv "R" [Branch "Date" "d" End])))
        (Send "R" "Quit" EUnit End))])]

-- R only handles ok, missing the quit branch the protocol can send
recvMissingR :: P
recvMissingR =
  Recv "P" [Branch "Title" "t"
    (Send "P" "Quote" (EReal 15.0)
      (Send "Q" "Quote" (EReal 15.0)
        (Recv "Q"
          [ Branch "Ok" "u"
              (Recv "Q" [Branch "Addr" "a"
                (Send "Q" "Date" (EStr "2026-06-12") End)]) ])))]

-- R has an extra branch for a label that can never arrive
recvExtraR :: P
recvExtraR =
  Recv "P" [Branch "Title" "t"
    (Send "P" "Quote" (EReal 15.0)
      (Send "Q" "Quote" (EReal 15.0)
        (Recv "Q"
          [ Branch "Ok" "u"
              (Recv "Q" [Branch "Addr" "a"
                (Send "Q" "Date" (EStr "2026-06-12") End)]),
            Branch "Quit" "u" End,
            Branch "Banana" "b" End ])))]

twoBuyerManualBad, twoBuyerBadPartner, twoBuyerBadLabel,
  twoBuyerEarlyEnd, twoBuyerRecvPartner, twoBuyerExtraSend,
  twoBuyerRecvWhereSend, twoBuyerUnbound, twoBuyerBadIf,
  twoBuyerRecvMissing, twoBuyerRecvExtra, twoBuyerExtraRole :: [(String, P)]
twoBuyerManualBad = [("P", manualPbad), ("Q", manualQ), ("R", manualR)]
twoBuyerBadPartner = [("P", badPartnerP), ("Q", manualQ), ("R", manualR)]
twoBuyerBadLabel = [("P", badLabelP), ("Q", manualQ), ("R", manualR)]
twoBuyerEarlyEnd = [("P", earlyEndP), ("Q", manualQ), ("R", manualR)]
twoBuyerRecvPartner = [("P", recvWrongPartnerP), ("Q", manualQ), ("R", manualR)]
twoBuyerExtraSend = [("P", extraSendP), ("Q", manualQ), ("R", manualR)]
twoBuyerRecvWhereSend = [("P", recvWhereSendP), ("Q", manualQ), ("R", manualR)]
twoBuyerUnbound = [("P", unboundP), ("Q", manualQ), ("R", manualR)]
twoBuyerBadIf = [("P", manualP), ("Q", badIfQ), ("R", manualR)]
twoBuyerRecvMissing = [("P", manualP), ("Q", manualQ), ("R", recvMissingR)]
twoBuyerRecvExtra = [("P", manualP), ("Q", manualQ), ("R", recvExtraR)]
twoBuyerExtraRole = twoBuyerManual ++ [("Z", End)]

-- Ping-Pong (recursion)

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

-- continue to a loop name that was never opened
badPingVarP :: P
badPingVarP =
  Rec "X0"
    (Send "Q" "Ping" EUnit
      (Recv "Q" [Branch "Pong" "p" (Var "WRONG")]))

pingPongManual, pingPongBadVar :: [(String, P)]
pingPongManual = [("P", manualPingP), ("Q", manualPingQ)]
pingPongBadVar = [("P", badPingVarP), ("Q", manualPingQ)]
