main = print True

data Expr = Lit Integer | Add Expr Expr

-- | Evaluate an addition AST
-- >>> eval (Add (Lit 1) (Lit 9001))
-- 9002
eval :: Expr -> Integer
eval (Lit int) = int
eval (Add a b) = eval a + eval b

-- | Print an addition AST
-- >>> printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
-- >>> let a1 = Add (Lit 9001) (Lit 1)
-- >>> let a2 = Add a1 (Lit 20001)
-- >>> let a3 = Add (Lit 1) a2
-- >>> printExpr a3
-- "1 + 9001 + 1 + 20001"
printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

