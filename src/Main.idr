module Main

import Data.String

import Lambda

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
printStack xs = putStrLn $ (addSpaceAfterIfNotEmpty (joinBy " " (map show (reverse xs)))) ++ "<- Top"
  where
  addSpaceAfterIfNotEmpty : String -> String
  addSpaceAfterIfNotEmpty "" = ""
  addSpaceAfterIfNotEmpty s = s ++ " "

repl : List Word -> IO ()
repl stack = do
  printStack stack
  input <- getLine
  newStack <- stackOnto (parse input) stack
  putStrLn "OK"
  repl newStack

main : IO ()
main = repl []
