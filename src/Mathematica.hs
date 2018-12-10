{-|
 - Module       : Mathematica
 - Description  : Mathematica utilities for υ-reduction grammars.
 - Copyright    : (c) Maciej Bendkowski, 2018
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module Mathematica where

import Prelude hiding (abs, succ)
import Control.Monad (guard, zipWithM)
import Data.Maybe

import Grammar

-- | Symbolic expressions.
data Exp =
    Exp { constant     :: Int         -- ^ constants.
        , monomial     :: Int         -- ^ monomials z^n.
        , termExp      :: Int         -- ^ monomials T(z)^n
        , subsExp      :: Int         -- ^ monomials S(z)^n
        , natExp       :: Int         -- ^ monomials N(z)^n
        , rewritingExp :: [(Int,Int)] -- ^ sorted list of monomials R(z)^{r_i}
        }

-- | Default, neutral expression.
defaultExp :: Exp
defaultExp =
    Exp { constant     = 1
        , monomial     = 0
        , termExp      = 0
        , subsExp      = 0
        , natExp       = 0
        , rewritingExp = []
        }

zExp :: Exp
zExp = defaultExp { monomial = 1 }

-- | Merges two sorted rewriting expression lists.
merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge [] xs = xs
merge xs [] = xs
merge xs @ ((r,n) : xs')  ys @ ((r',n') : ys')
    | r == r'   = (r, n + n') : merge xs' ys'
    | r < r'    = (r,n) : merge xs' ys
    | otherwise = (r',n') : merge xs ys'

-- | Adds two expressions together.
join :: Exp -> Exp -> Exp
join e e' =
    Exp { constant     = constant e * constant e'
        , monomial     = monomial e + monomial e'
        , termExp      = termExp e + termExp e'
        , subsExp      = subsExp e + subsExp e'
        , natExp       = natExp e + natExp e'
        , rewritingExp = merge (rewritingExp e)
                               (rewritingExp e')
        }

-- | Turns a variable into an expression.
varExp :: Variable -> Exp
varExp T     = defaultExp { termExp      = 1 }
varExp S     = defaultExp { subsExp      = 1 }
varExp N     = defaultExp { natExp       = 1 }
varExp (G n) = defaultExp { rewritingExp = [(n,1)] }

-- | Turns a term into an expression.
toExp :: Term -> Exp
toExp (Var v)    = varExp v
toExp (Fun _ ts) = foldl join zExp ts'
    where ts' = map toExp ts

------------------------------------------------------------
-- Printing utilities obeying Mathematica's input format. --
------------------------------------------------------------

showZ :: Int -> ShowS
showZ 0 = (++) ""
showZ 1 = (++) "*z"
showZ n = (++) "*z^{" . shows n . (++) "}"

showT :: Int -> ShowS
showT 0 = (++) ""
showT 1 = (++) "*T[z]"
showT n = (++) "*(T[z])^{" . shows n . (++) "}"

showS :: Int -> ShowS
showS 0 = (++) ""
showS 1 = (++) "*S[z]"
showS n = (++) "*(S[z])^{" . shows n . (++) "}"

showN :: Int -> ShowS
showN 0 = (++) ""
showN 1 = (++) "*B[z]"
showN n = (++) "*(B[z])^{" . shows n . (++) "}"

showG :: [(Int, Int)] -> ShowS
showG [] = (++) ""
showG ((r,1) : rs) = (++) "*Subscript[G,"
    . shows r . (++) "][z]"
    . showG rs

showG ((r,p) : rs) = (++) "*(Subscript[G,"
    . shows r  . (++) "][z])^{"
    . shows p . (++) "}"
    . showG rs

showExp :: Exp -> ShowS
showExp e
    | c == 0 = (++) "0"
    | k == 0 && t == 0 && s == 0 && n == 0 && null r = shows c
    | otherwise = shows c . showZ k . showT t . showS s . showN n . showG r
    where c = constant e
          k = monomial e
          t = termExp e
          s = subsExp e
          n = natExp e
          r = rewritingExp e

-- | Determines if the given term
--   contains an occurrence of G_n.
selfref :: Int -> Term -> Bool
selfref n (Fun _ ts) = any (selfref n) ts
selfref n (Var (G m)) = n == m
selfref _ _ = False

showGrammar :: [Term] -> ShowS
showGrammar = showGrammar' . map toExp

showGrammar' :: [Exp] -> ShowS
showGrammar' [] = (++) ""
showGrammar' [e] = showExp e
showGrammar' (e : es) = showExp e . (++) " + " . showGrammar' es

-- | Outputs the symbolic expression defining G_n(z),
--   i.e. the generating function corresponding to G_n.
toMathematica :: Int -> String
toMathematica 0 = "(1-z-Sqrt[(1-3*z-z^2-z^3)/(1-z)])/(2*z)"
toMathematica n = "(" ++ x ++ ")/(Sqrt[(1-3*z-z^2-z^3)/(1-z)])"
    where x = showGrammar (filter (not . selfref n) $ grammars !! n) ""

toMathematica' :: Int -> String
toMathematica' 0 = "(1-z-Sqrt[(1-3*z-z^2-z^3)/(1-z)])/(2*z)"
toMathematica' n = "(" ++ x ++ ")/(Sqrt[(1-3*z-z^2-z^3)/(1-z)])"
    where x = showGrammar (filter (not . selfref n) $ grammars' !! n) ""
