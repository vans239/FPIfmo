{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.REPL where

import Prelude;
import Data.List.Utils
import Data.String.Utils

data Lambda = LV Variable | L Variable Lambda | Variable Variable

eval 
{-
data Lambda = Exp [String] | Lambda String Lambda deriving (Read, Show)
data Expression = Expression Lambda [String] deriving (Read, Show)

calculate :: Expression -> String
calculate (Expression  lambda arguments) = reduction lambda [] arguments

reduction :: Lambda -> [(String, String)] -> [String] -> String
reduction (Exp tokens) replacements arguments = replaceExp tokens replacements where
          replaceExp tokens [] = join " " tokens
          replaceExp tokens ((from, to) : other) = replaceExp (map fun tokens) other where 
                     fun a | (a == from) = to
                           | otherwise = a
reduction (Lambda s lambda) replacements (arg : args) = 
          reduction lambda ((s, arg) : replacements) args  
-} 

--test = eval "3+4"
--test = runInterpreter $ setImports ["Prelude"] >> eval "3 + 5" 
