module Lambda

public export
data Term
  = Var Nat
  | Lam Term
  | App Term Term

Eq Term where
  (Var n) == (Var n') = n == n'
  (Lam t) == (Lam t') = t == t'
  (App t1 t2) == (App t1' t2') = t1 == t1' && t2 == t2'
  _ == _ = False

substitute : Term -> Term -> Term
substitute t1 t2 = substitute' 0 t1 t2
  where
  substitute' : Nat -> Term -> Term -> Term
  substitute' n (Var n') t2 = if n == n' then t2 else (Var n')
  substitute' n (Lam t1) t2 = Lam $ substitute' (S n) t1 t2
  substitute' n (App tl tr) t2 = App (substitute' n tl t2) (substitute' n tr t2)

-- normalizing order, leftmost - outermost reduction
step : Term -> (Bool, Term)
step (Var n) = (False, Var n)
step (Lam t) = let (substituted, t') = step t in (substituted, Lam t')
step (App (Lam t1) t2) = (True, substitute t1 t2)
step (App t1 t2) =
  let (substituted, t1') = step t1 in
  if substituted
    then (True, App t1' t2)
    else (
      let (substituted', t2') = step t2 in
      if substituted'
        then (True, App t1 t2')
        else (False, App t1 t2)
    )

-- keep stepping until there are no more substitutions
export
reduce : Term -> Term
reduce t = let (substituted, t') = step t in if substituted then reduce t' else t
