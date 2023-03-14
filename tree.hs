data Expr
  = Add Expr Expr  -- addition operation
  | Val Int        -- integer value
eval :: Expr -> Int
eval (Add e1 e2) = eval e1 + eval e2
eval (Val n) = n



main = do
  putStrLn "let's build an AST"
  print(eval (Add (Val 1) (Val 2)))