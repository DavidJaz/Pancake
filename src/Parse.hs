module Parse (readLine) where

import Cmds

import Flow
import Text.Parsec (ParseError, parse, try, sepBy, lookAhead, notFollowedBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, satisfy, string)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$), liftA2)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import Data.String (words, unwords)
import Data.List (reverse)
import Data.List.Split (splitOn)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

end_word :: Parser ()
end_word = do lookAhead (oneOf " \n\t;{")
              return ()

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
  
num :: Parser Value
num = LitInt <$> read <$> lexeme (many1 digit) 

name_symbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*_+=-~|<>"

name :: Parser Name
name = (many1 $ oneOf name_symbols)

vname :: Parser Value
vname = VName <$> ((char '\'') *> name ) 

quote :: Parser Value
quote = Quote <$> (
          lexeme (char '{')
          *> blocks
          <* lexeme (char '}')
          )

vstring :: Parser Value
vstring = LitString <$>
            (
              lexeme (char '\"')
              *> many (satisfy (/= '\"'))
              <* lexeme (char '\"')
            )


tint :: Parser Value
tint = lexeme (string "Int") *> return TInt

tstring :: Parser Value
tstring = lexeme (string "String") *> return TString

ttype :: Parser Value
ttype = lexeme (string "Type") *> return  VType
  
value :: Parser Value
value =  num
     <|> vname
     <|> quote
     <|> vstring
     <|> try tint
     <|> try tstring
     <|> try ttype

push :: Parser Cmd
push = Push <$> lexeme value

from_cmd :: Cmd -> Parser Cmd
from_cmd c = lexeme (string (to_string c) >> notFollowedBy name) *> return c

call :: Parser Cmd
call = Call <$> name

rule :: Parser Cmd
rule =
  do
    _ <- lexeme (char '(')
    xs <- many value
    _ <- lexeme (string "->")
    ys <- many value
    _ <- lexeme (char ')')
    return $ Rule xs ys

cmd :: Parser Cmd
cmd = lexeme $ try push
            <|> try (from_cmd CAdd)
            <|> try (from_cmd CSub)
            <|> try (from_cmd CMul)
            <|> try (from_cmd CDiv)
            <|> try (from_cmd CExp)
            <|> try (from_cmd CDo)
            <|> try (from_cmd CLet)
            <|> try (from_cmd CDup)
            <|> try (from_cmd CDip)
            <|> try (from_cmd CPop)
            <|> try (from_cmd CQuote)
            <|> try (from_cmd CComp)
            <|> try (from_cmd CImport)
            <|> try (from_cmd CMatch)
            <|> try (from_cmd CHead)
            <|> try (from_cmd CEq)
            <|> try rule
            <|> call

cmds :: Parser Cmds
cmds = whitespace *> many cmd

block :: Parser Cmds
block = reverse <$> cmds 

blocks :: Parser Cmds
blocks = concat <$> ( sepBy block (char ';')  )
  

readLine :: String -> Error Cmds
readLine = parse blocks ""
