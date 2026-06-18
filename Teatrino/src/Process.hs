module Process where

data Expr = EVar String
          | EInt Integer
          | EReal Double
          | EStr String
          | EBool Bool
          | EUnit
          deriving Eq

-- Print expressions the way you would write them in a process file,
-- not as Haskell constructors (so "true" rather than "EBool True").
instance Show Expr where
  show (EVar x) = x
  show (EInt n) = show n
  show (EReal d) = show d
  show (EStr s) = show s
  show (EBool True) = "true"
  show (EBool False) = "false"
  show EUnit = "()"


data Branch = Branch {
    bLabel :: String,
    bVar   :: String,
    bCont  :: P
  } deriving Show


data P = Send String String Expr P
       | Recv String [Branch]
       | If Expr P P
       | Rec String P
       | Var String
       | End
       deriving Show

-- Print a process back in the file syntax, with indentation.
ppProc :: P -> String
ppProc = pp 2 where
  pp :: Int -> P -> String
  pp ind (Send q l e k) =
    pfx ind ++ q ++ "!" ++ l ++ "(" ++ show e ++ ");\n" ++ pp ind k
  -- a receive with a single branch prints in the short form q?l(x);
  pp ind (Recv q [Branch l x k]) =
    pfx ind ++ q ++ "?" ++ l ++ "(" ++ x ++ ");\n" ++ pp ind k
  pp ind (Recv q bs) =
    pfx ind ++ q ++ "?{\n" ++ unlines (map f bs) ++ pfx ind ++ "}"
    where
      f (Branch l x k) = pfx (ind + 2) ++ l ++ "(" ++ x ++ ") {\n"
        ++ pp (ind + 4) k ++ "\n" ++ pfx (ind + 2) ++ "}"
  pp ind (If e p1 p2) =
    pfx ind ++ "if " ++ show e ++ " {\n" ++ pp (ind + 2) p1 ++ "\n"
      ++ pfx ind ++ "} else {\n" ++ pp (ind + 2) p2 ++ "\n" ++ pfx ind ++ "}"
  pp ind (Rec x k) =
    pfx ind ++ "rec " ++ x ++ " {\n" ++ pp (ind + 2) k ++ "\n" ++ pfx ind ++ "}"
  pp ind (Var x) = pfx ind ++ "continue " ++ x ++ ";"
  pp ind End = pfx ind ++ "end"

  pfx :: Int -> String
  pfx n = replicate n ' '
