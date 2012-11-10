
module BasicParser
   where

import Monstupar.Core
import Monstupar.Derived

data BasicExpr = Ignored Expression 
               | Assigned Variable Expression 
               | While Expression [BasicExpr]
               | If Expression [BasicExpr] [BasicExpr] deriving (Show, Read, Eq)
data Expression = Var Variable | Const Integer deriving (Show, Read, Eq)
type Variable = String 

constant :: Monstupar Char Expression  
constant = (do 
             e <- many1 digit
             return (Const (read e))
           ) 

variable :: Monstupar Char Variable  
variable = (do
             e <- letter
             es <- many (notOneOf " ;")
             return (e : es)
           )

onspace = many (oneOf " \n")

expression :: Monstupar Char Expression  
expression =  (do
                e <- variable
                return (Var e)
              ) <|> constant

assignedBasicExpr :: Monstupar Char BasicExpr  
assignedBasicExpr = ( do
                      onspace
                      e <- variable
                      onspace
                      char '='
                      onspace
                      es <- expression
                      onspace 
                      char ';'
                      return (Assigned e es)
                   )

ignoredBasicExpr :: Monstupar Char BasicExpr 
ignoredBasicExpr = ( do
               onspace
               e <- expression
               onspace
               char ';'
               return (Ignored e)
             )

whileBasicExpr :: Monstupar Char BasicExpr 
whileBasicExpr =  ( do
               onspace
               string "while"
               onspace
               char '('
               onspace
               e <- expression
               onspace
               char ')'
               onspace
               char '{'
               onspace
               es <- separated basicExpr onspace
               onspace
               char '}'
               return (While e es) 
             )

ifBasicExpr :: Monstupar Char BasicExpr
ifBasicExpr = (do
               onspace
               string "if"
               onspace
               char '('
               onspace
               e <- expression
               onspace
               char ')'
               onspace

               string "then"
               onspace
               char '{'
               onspace
               a <- separated basicExpr onspace
               onspace
               char '}'

               onspace
               string "else"
               onspace
               char '{'
               onspace
               b <- separated basicExpr onspace
               onspace
               char '}'
               onspace
               return (If e a b) 
             )

basicExpr = assignedBasicExpr <|> ignoredBasicExpr <|> whileBasicExpr <|> ifBasicExpr

programmExpr :: Monstupar Char [BasicExpr]
programmExpr = separated basicExpr onspace

testParser s = runParser programmExpr s

tests = [t1,t2,t3, t4, t5, t6, t7, t8]
check = (filter (== False) tests) == [] 

t1 = runParser constant "1" == Right ("",Const 1)
t2 = runParser variable "abcd1 " == Right (" ","abcd1")
t3 = runParser expression "22342352 3" == Right (" 3",Const 22342352)
t4 = runParser assignedBasicExpr " a = 1;" == Right ("",Assigned "a" (Const 1)) 
t5 = runParser ignoredBasicExpr " 2345 ;" == Right ("",Ignored (Const 2345))
t6 = runParser whileBasicExpr " while ( 3) { 2; 5; a=4;}" == Right ("",While (Const 3) [Ignored (Const 2),Ignored (Const 5),Ignored (Var "a=4")])

t7 = testParser "a=4; 4; 7; while (6) { i   = 3; }" == Right ("",[Ignored (Var "a=4"),Ignored (Const 4),Ignored (Const 7),While (Const 6) [Assigned "i" (Const 3)]])

t8 = runParser ifBasicExpr " if ( 3) then { a =5; c =2;} else {} " == Right ("",If (Const 3) [Assigned "a" (Const 5),Assigned "c" (Const 2)] [])

--new = runParser " if ( 3) then { a =5; c =2;} else {} " 

