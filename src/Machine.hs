module Machine where

import Cmds
import Parse 

import Flow
import qualified Data.Map.Strict as Map
import Text.Parsec
import Control.Applicative
import Data.Maybe (fromMaybe)

type MachineState = (Environment, Values)

begin :: MachineState
begin = (Map.empty, [])

load :: Error Cmds -> Error MachineState
load = fmap $ run_all begin

run :: Cmd -> MachineState -> MachineState
run (Push v) (e, stack) = (e,v:stack)
run CAdd     (e, ((LitInt x):(LitInt y):rest)) = (e, (LitInt (x + y)):rest)
run CSub     (e, ((LitInt x):(LitInt y):rest)) = (e, (LitInt (y - x)):rest)
run CMul     (e, ((LitInt x):(LitInt y):rest)) = (e, (LitInt (x * y)):rest)
run CDiv     (e, ((LitInt x):(LitInt y):rest)) = (e, (LitInt (y `div` x)):rest)
run CExp     (e, ((LitInt x):(LitInt y):rest)) = (e, (LitInt (y ^ x)):rest)
run CDo      (e, ((Quote q):rest))             =   run_all (e, rest) q
run CLet     (e, (VName n):((Quote q):rest))   = (Map.insert n q e, rest)
run (Call n) (e, stack)                        = run_all (e, stack) $ fromMaybe [] (Map.lookup n e)
run (Rule xs ys) (e, stack)                    =
  case takePrefix xs stack of
    Just rest -> (e, ys ++ rest)
    Nothing   -> (e, stack)
  where
    takePrefix :: Eq a => [a] -> [a] -> Maybe [a]
    takePrefix [] ys = Just ys
    takePrefix (x:xs) (y:ys) =
      if x == y
      then takePrefix xs ys
      else Nothing
run CDup   (e, (x:rest))                       = (e, x:x:rest)
run CDip   (e, ((Quote q):x:rest)) = onRight ((:) x) (run_all (e, rest) q)
  where
    onRight :: (a -> b) -> (c, a) -> (c, b)
    onRight f (c, a) = (c, f a)
run CPop   (e, (_:rest)) = (e, rest)
run CQuote (e, (x:rest)) = (e, (Quote [Push x]):rest)
run CComp  (e, ((Quote a):(Quote b):rest)) = (e, (Quote $ a ++ b):rest)
run CMatch (e, ((VName n):(VName m):rest)) =
  if n == m
  then (e, rest)
  else (e, (VName m):rest)
run CHead (e, ((Quote (q:qs)):rest )) = (e, (Quote [q]):(Quote qs):rest)
run CEq   (e, (x:y:rest)) =
  if x == y
  then (e, ((Quote [Push (Quote [CPop]), CDip]):x:y:rest))
  else (e, ((Quote [CPop]):x:y:rest))
  
run_all :: MachineState -> Cmds -> MachineState
run_all = foldl (flip run) 

machine :: Error (MachineState) -> [Error Cmds] -> [Error (MachineState)]
machine = scanl $ liftA2 run_all

pretty_print :: Error MachineState -> String
pretty_print (Left e) = show e
pretty_print (Right (e, stack)) = print_stack stack

repl :: Error Cmds -> IO ()
repl imports = 
    interact $
         lines
      .> map readLine
      .> machine (load imports)
      .> map pretty_print
      .> unlines
