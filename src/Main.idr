module Main

import Data.String

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
reduce : Term -> Term
reduce t = let (substituted, t') = step t in if substituted then reduce t' else t

data Word
  = WReduce
  | WDup
  | WVar Nat
  | WLam
  | WApp
  | WTerm Term
  | WPrint

Show Word where
  show WReduce = "r"
  show WDup = "dup"
  show (WVar n) = show n
  show WLam = "λ"
  show WApp = "a"
  show (WTerm _) = "t"
  show WPrint = "."

parse : String -> List Word
parse input = map parseWord (words input)
  where
  parseWord : String -> Word
  parseWord "λ" = WLam
  parseWord "a" = WApp
  parseWord "." = WPrint
  parseWord "r" = WReduce
  parseWord "dup" = WDup
  parseWord v = WVar (stringToNatOrZ v)

termString : Term -> String
termString term = joinBy " " (termString' term)
  where
  termString' : Term -> List String
  termString' (Var n) = [show n]
  termString' (Lam t) = termString' t ++ ["λ"]
  termString' (App t1 t2) = termString' t2 ++ termString' t1 ++ ["a"]

printExpandedWord : Word -> IO ()
printExpandedWord (WTerm term) = putStrLn $ termString term
printExpandedWord word = printLn word

foldIntoWTerm : List Word -> IO (List Word)
foldIntoWTerm ((WVar n) :: words) = pure $ WTerm (Var n) :: words
foldIntoWTerm (WLam :: (WTerm t) :: words) = pure $ WTerm (Lam t) :: words
foldIntoWTerm (WApp :: (WTerm t1) :: (WTerm t2) :: words) = pure $ WTerm (App t1 t2) :: words
foldIntoWTerm (WDup :: word :: words) = pure $ word :: word :: words
foldIntoWTerm (WPrint :: word :: words) = printExpandedWord word >> pure words
foldIntoWTerm (WReduce :: (WTerm t) :: words) = pure $ WTerm (reduce t) :: words
foldIntoWTerm words = pure words

stackOnto : List Word -> List Word -> IO (List Word)
stackOnto (x :: xs) onto = foldIntoWTerm (x :: onto) >>= (stackOnto xs)
stackOnto _ onto = pure onto

printStack : List Word -> IO ()
printStack xs = putStrLn $ (addSpaceAterIfNotEmpty (joinBy " " (map show (reverse xs)))) ++ "<- Top"
  where
  addSpaceAterIfNotEmpty : String -> String
  addSpaceAterIfNotEmpty "" = ""
  addSpaceAterIfNotEmpty s = s ++ " "

repl : List Word -> IO ()
repl stack = do
  printStack stack
  input <- getLine
  newStack <- stackOnto (parse input) stack
  putStrLn "OK"
  repl newStack

main : IO ()
main = repl []
