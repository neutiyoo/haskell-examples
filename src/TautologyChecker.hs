-- proposition
data Prop
  = Const Bool
  | Var   Char
  | Not   Prop
  | And   Prop Prop
  | Or    Prop Prop
  | Imply Prop Prop
  | Eq    Prop Prop

-- substitution as a lookup table
-- e.g. [('A',False) , ('B',True)]
type Subst = Assoc Char Bool

type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- |  A  |  B  | ¬A  | A ∧ B | A ∨ B | A ⇒ B | A ⇔ B |
-- | :-: | :-: | :-: | :---: | :---: | :---: | :---: |
-- |  F  |  F  |  T  |   F   |   F   |   T   |   T   |
-- |  F  |  T  |  T  |   F   |   T   |   T   |   F   |
-- |  T  |  F  |  F  |   F   |   T   |   F   |   F   |
-- |  T  |  T  |  F  |   T   |   T   |   T   |   T   |

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Eq p q)    = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Eq p q)    = vars p ++ vars q

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (/= x) xs)

uniqueVars = unique . vars

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bools (n - 1) ++ map (True:) bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = uniqueVars p

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- unit test

type Expected = Bool
type TestInput = Prop
testCases :: [(Expected,TestInput)]
testCases = [
  (False, And (Var 'A') (Not (Var 'A'))),
  (False, And (Var 'A') (Not (Var 'A'))),
  (True,  Imply (And (Var 'A') (Var 'B')) (Var 'A')),
  (False, Imply (Var 'A') (And (Var 'A') (Var 'B'))),
  (True,  Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')),
  (True,  Or (Var 'A') (Not (Var 'A'))),
  (False, Or (Var 'A') (Var 'A')),
  (False, Eq (Var 'A') (Not (Var 'A'))),
  (True,  Eq (Var 'A') (Var 'A'))
  ]

throw c e r = do error ("case:" ++ show c ++ " expected:" ++ show e ++ " received:" ++ show r)
test f ts = sequence_ [ throw c e (f x) | (c, (e, x)) <- zip [1..] ts , e /= (f x)]

main = test isTaut testCases
