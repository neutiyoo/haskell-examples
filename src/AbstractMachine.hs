-- expression
data Expr = Val Int | Add Expr Expr

-- control stack
type Cont = [Op]

-- operation
data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value :: Expr -> Int
value e = eval e []


-- mutually recursive functions: eval, exec

-- (1 + 2) + 4
-- value (Add (Add (Val 1) (Val 2)) (Val 4))
-- eval (Add (Add (Val 1) (Val 2)) (Val 4)) []
-- eval (Add (Val 1) (Val 2)) [EVAL (Val 4)]
-- eval (Val 1) [EVAL (Val 2), EVAL (Val 4)]
-- exec [EVAL (Val 2), EVAL (Val 4)] 1
-- eval (Val 2) [ADD 1, EVAL (Val 4)]
-- exec [ADD 1, EVAL (Val 4)] 2
-- exec [EVAL (Val 4)] 3
-- eval (Val 4) [ADD 3]
-- exec [ADD 3] 4
-- exec [] 7

-- unit test

type Expected = Int
type TestInput = Expr
testCases :: [(Expected,TestInput)]
testCases = [
  (3, Add (Val 1) (Val 2)),
  (9, Add (Add (Val 2) (Val 3)) (Val 4))
  ]

throw c e r = do error ("case:" ++ show c ++ " expected:" ++ show e ++ " received:" ++ show r)
test f ts = sequence_ [ throw c e (f x) | (c, (e, x)) <- zip [1..] ts , e /= (f x)]

main = test value testCases
