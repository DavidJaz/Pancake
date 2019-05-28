module Types where


import Cmds

import Control.Monad

data TypeError = Mismatch Value Value
               | AndAlso TypeError TypeError
               | BlankError

instance Show TypeError where
  show (Mistmatch a b) = "Type Error: I was expecting a\n\t" ++ (show a) ++ "\nBut I got a\n\t" ++ (show b)
  show (AndAlso a b) = show a ++ "\n\nAnd, also:\n\n" ++ show b
  show BlankError = ""

instance Semigroup TypeError where
  (<>) = AndAlso

instance Monoid TypeError where
  mempty = AnyError


instance MonadPlus (Either TypeError) where
  mzero = Left mempty
  
  
infer :: Value -> Either TypeError Type
infer (LitInt _) = Right TInt
infer (Quote cmds) = Quote <$> (infer_cmds cmds)
infer (VName _) = Right TName
infer VType = Right VType
infer TInt  = Right VType
infer TString = Right TString

infer_cmds :: Cmds -> Either TypeError Cmds
infer_cmds = foldM acc []
  where
    acc :: Cmds -> Cmd -> Either TypeError Cmds
    acc (Push v) = Push <$> infer v
    acc (CAdd) = Rule [TInt, TInt] [TInt]
    acc (CMul) = Rule [TInt, TInt] [TInt]
    acc (CSub) = Rule [TInt, TInt] [TInt]
    acc (CDiv) = Rule [TInt, TInt] [TInt]
    acc (CExp) = Rule [TInt, TInt] [TInt]
    acc (CDo)  = CDo
    acc (CLet) = CPop
    acc (Call n) = CPop -- PROBLEM NEED TO DO THIS WITH A CONTEXT
    acc (Rule xs ys) = Rule (map infer xs) (map infer ys)
    acc CDup = CDup
    acc CDip = CDip
    acc CPop = CPop
    acc CQuote = CQuote
    acc CComp = CComp
    acc CImport = CPop -- This currently has no semantics
    acc CMatch = undefined
    acc CHead = undefined
    acc CEq = undefined -- I need to add a type of booleans.

  
