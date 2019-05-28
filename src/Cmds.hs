module Cmds where

import Flow
import qualified Data.Map.Strict as Map
import Data.String
import Text.Parsec

-- comment
type Stack a = [a]

print_stack :: Show a => Stack a -> String
print_stack = map show .> unwords

type Identifier = Int
type Name = String
type Environment = Map.Map Name Cmds

empty_env :: Environment
empty_env = Map.empty

type Error a = Either ParseError a

data Cmd = Push Value
         | CAdd
         | CSub
         | CMul
         | CDiv
         | CExp
         | CDo
         | CLet
         | Call Name
         | Rule (Stack Value) (Stack Value)
         | CDup
         | CDip
         | CPop
         | CQuote
         | CComp
         | CImport
         | CMatch
         | CHead
         | CEq
         | CMap
  deriving (Show, Eq)

to_string :: Cmd -> String
to_string (Push v) = show v
to_string CAdd     = "+"
to_string CSub     = "-"
to_string CMul     = "*"
to_string CDiv     = "/"
to_string CExp     = "**"
to_string CDo      = "do"
to_string CLet     = "let"
to_string (Call n) = n
to_string (Rule xs ys) = "(" ++ (print_stack xs) ++ " -> " ++ (print_stack ys) ++ ")"
to_string CDup     = "dup"
to_string CDip     = "dip"
to_string CPop     = "pop"
to_string CQuote   = "quote"
to_string CComp    = "comp"
to_string CImport  = "import"
to_string CMatch   = "match"
to_string CHead    = "head"
to_string CEq      = "="

print_cmds :: Cmds -> String
print_cmds = map to_string .> unwords

data Value = LitInt Int
           | LitString String
           | Quote (Stack Cmd)
           | VName Name
           | VType
           | TInt
           | TString
  deriving (Eq)

instance Show Value where
  show (LitInt n) = show n
  show (LitString s) = "\"" ++ s ++ "\""
  show (Quote stack) = print_cmds stack
  show (VName n) = n
  show VType = "Type"
  show TInt  = "Int"
  show TString = "String"

type Cmds = Stack Cmd
type Values = Stack Value

