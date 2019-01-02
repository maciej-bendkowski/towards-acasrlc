{-|
 - Module       : Grammar
 - Description  : υ-reduction grammars.
 - Copyright    : (c) Maciej Bendkowski, 2018
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module Grammar where

import Prelude hiding (abs, succ)
import Control.Monad (guard, zipWithM)
import Data.Maybe

-- | Regular tree grammar terms.
data Term = Fun Function [Term]
          | Var Variable
          deriving (Eq)

parens :: String -> String
parens x = "(" ++ x ++ ")"

brackets :: String -> String
brackets x = "[" ++ x ++ "]"

-- | Pritty-printer for terms.
instance Show Term where
        show (Fun App [a,b]) = show a ++ parens (show b)
        show (Fun Abs [a]) = "λ" ++ parens (show a)
        show (Fun Closure [a,s]) = parens (show a) ++ brackets (show s)
        show (Fun Slash [a]) = show a ++ "/"
        show (Fun Lift [s]) = "⇑" ++ parens (show s)
        show (Fun Shift []) = "↑"
        show (Fun Succ [n]) = "S" ++ parens (show n)
        show (Fun Zero []) = "0"

        show (Var T) = "T"
        show (Var S) = "S"
        show (Var N) = "N"
        show (Var (G n)) = "G_" ++ show n

        show _ = error "Absurd case."

-- | Non-terminal symbols.
data Variable = T     -- ^ λυ-terms.
              | S     -- ^ substitutions.
              | N     -- ^ de Bruijn indices.
              | G Int -- ^ reduction grammars.
              deriving (Eq, Show)

-- | Partial order of sorts (non-terminals).
--   Returns the greatest lower bound or Nothing
--   if both variables are incomparable.
inf :: Variable -> Variable -> Maybe Variable
inf T x     = return x
inf x T     = return x
inf (G 0) N = return N
inf N (G 0) = return N
inf x y     =
    if x == y then return x
              else Nothing

-- | Function symbols in the language of λυ-terms.
data Function = App       -- ^ term application.
              | Abs       -- ^ term abstraction λ.
              | Closure   -- ^ closure [].
              | Slash     -- ^ slash /.
              | Lift      -- ^ lift ⇑.
              | Shift     -- ^ shift ↑.
              | Succ      -- ^ successor S.
              | Zero      -- ^ zero 0.
                deriving (Eq, Show)

-- | Opaque term application.
app :: Term -> Term -> Term
app t t' = Fun App [t, t']

-- | Opaque term abstraction.
abs :: Term -> Term
abs t = Fun Abs [t]

-- | Opaque closure.
closure :: Term -> Term -> Term
closure t s = Fun Closure [t, s]

-- | Opaque slash.
slash :: Term -> Term
slash t = Fun Slash [t]

-- | Opaque lift.
lift :: Term -> Term
lift s = Fun Lift [s]

-- | Opaque shift.
shift :: Term
shift = Fun Shift []

-- | Opaque successor.
succ :: Term -> Term
succ n = Fun Succ [n]

-- | Opaque zero.
zero :: Term
zero = Fun Zero []

-- | 'Weak' function symbol equality.
weq :: Term -> Term -> Bool
weq (Var x) (Var y) = x == y
weq (Fun f xs) (Fun g ys) =
    f == g && length xs == length ys
weq  _ _ = False

-- | Function predicate.
isFunction :: Term -> Bool
isFunction (Fun _ _) = True
isFunction _         = False

-- | Finite intersection partitions.
--   Returns a list of terms forming the partition,
--   or, if the outcome FIP is empty, Nothing.
fip :: Term -> Term -> Maybe [Term]
fip t @ (Fun f xs) t' @ (Fun _ ys) = do
    guard (t `weq` t') -- function symbols have to be equal
    args <- zipWithM fip xs ys
    return [Fun f x | x <- sequence args]

fip (Var v) (Var v') = do
    x <- inf v v' -- takes the gretest lower bound.
    return [Var x]

fip f @ (Fun _ _) x @ (Var _) =
    fip x f -- flip arguments.

fip (Var T) f @ (Fun _ _) =
    return [f] -- note: a slight optimisation.

fip (Var v) f @ (Fun _ _) =
    let x = concat $ mapMaybe (fip f) (get v)
     in if null x then Nothing
                  else return x

-- | Getter for the given non-terminal productions.
get :: Variable -> [Term]
get T = [app (Var T) (Var T), abs (Var T),
            closure (Var T) (Var S), Var N]

get S = [slash (Var T), lift (Var S), shift]
get N = [succ (Var N), zero]

get (G n) = grammars !! n

-- | Term closure width.
closureWidth :: Term -> Int
closureWidth (Fun Closure [a, _]) = 1 + closureWidth a
closureWidth _ = 0

-- |  φ-matchings for the given template and term.
matchings :: Term -> Term -> Maybe [Term]
matchings template prod = fip x prod
    where w = closureWidth prod
          d = closureWidth template
          x = pad (w - d) template

-- | 'Padds' the given term tail with S
--   so to obtain the given closure width.
pad :: Int -> Term -> Term
pad 0 x = x
pad n x = Fun Closure [x',Var S]
    where x' = pad (n-1) x

-- | Decomposes a term into its head and tail.
decompose :: Term -> (Term, [Term])
decompose x = (h, reverse ts)
    where (h, ts) = decompose' x

decompose' :: Term -> (Term, [Term])
decompose' (Fun Closure [a,s]) = (h, s : ts)
    where (h,ts) = decompose' a

decompose' x = (x, [])

-- | Convenient helper for the construction of terms.
build :: Term -> [Term] -> Term
build = foldl closure

-- | Grammar G_0 for 'pure' terms.
r0 :: [Term]
r0  = [app (Var $ G 0) (Var $ G 0),
       abs (Var $ G 0), Var N]

grammars :: [[Term]]
grammars = r0 : nextG 1 grammars
    where nextG n (g : gs) = gen n g : nextG (n + 1) gs
          nextG _ _ = error "Absurd case"

-- | Extracts asymptotically significant
--   productions of reduction grammars.
grammars' :: [[Term]]
grammars' = map (filter large) grammars

-- | Checks if a given set of terms is unambiguous or not.
--   For that purpose, it is checked if all pairs of distinct terms
--   have empty finite intersection partitions.
unambiguous :: [Term] -> Bool
unambiguous g = f [fip t t' | t <- g, t' <- g, t /= t']
    where f [] = True
          f (Nothing : xs) = f xs
          f (Just _ : _) = False

-- | Asymptotically significant terms (productions).
large :: Term -> Bool
large (Fun _ ts)  = any large ts
large (Var N)     = False
large (Var (G 0)) = False
large _           = True

-- | Given an index n and all the grammars G_0,...,G_{n-1},
--   generates productions in the next, (n+1)st grammar.
gen :: Int -> [Term] -> [Term]
gen n g = shortProductions n
    ++ concat (mapMaybe appR g)            -- (App)
    ++ concat (mapMaybe lambdaR g)         -- (Lambda)

    -- note: a slight optimisation
    ++ [closure zero (slash $ Var $ G (n - 1))]

    ++ concat (mapMaybe fvarR g)           -- (FVar)

    ++ concat (mapMaybe rvarR g)           -- (RVar)
    ++ concat (mapMaybe fvarLiftR g)       -- (FVarLift)
    ++ concat (mapMaybe rvarLiftR g)       -- (RVarLift)
    ++ concat (mapMaybe varShiftR g)       -- (VarShift)

-- | Generates short production for G_n.
shortProductions :: Int -> [Term]
shortProductions n =
    abs (Var $ G n) : [app (Var $ G k) (Var $ G (n-k)) | k <- [0..n] ]

-- | (App) rule productions extracted
--   out of the given production.
appR :: Term -> Maybe [Term]
appR p = do
    let template = app (closure (Var T) (Var S)) (closure (Var T) (Var S))
    xs <- matchings template p
    return $ concat $ mapMaybe genAppR xs

genAppR :: Term -> Maybe [Term]
genAppR x =
    case decompose x of
      (Fun App [Fun Closure [a,s],Fun Closure [b,s']], ts) -> do
          fs <- fip s s'
          return [build (closure (app a b) f) ts | f <- fs]
      _ -> error "Invalid term in App"

-- | (Lambda) rule productions extracted
--   out of the given production.
lambdaR :: Term -> Maybe [Term]
lambdaR p = do
    let template = abs (closure (Var T) (lift $ Var S))
    xs <- matchings template p
    return $ map genLambdaR xs

genLambdaR :: Term -> Term
genLambdaR x =
    case decompose x of
      (Fun Abs [Fun Closure [a, Fun Lift [s]]], ts) ->
          build (closure (abs a) s) ts
      _ -> error "Invalid term in Lambda"

-- | (FVar) rule productions extracted
--   out of the given production.
fvarR :: Term -> Maybe [Term]
fvarR p = return
        [build (closure zero (slash x)) xs
              | (x,xs) <- splits h ts, not (null xs)] -- note: optimisation in gen!

    where (h, ts) = decompose p

splits :: Term -> [Term] -> [(Term, [Term])]
splits h [] = [(h,[])]
splits h (t : ts) =
    (h, t : ts) : splits (closure h t) ts

-- | (RVar) rule productions extracted
--   out of the given production.
rvarR :: Term -> Maybe [Term]
rvarR p = do
    let template = Var N
    xs <- matchings template p
    return $ map genRVarR xs

genRVarR :: Term -> Term
genRVarR x =
    case decompose x of
      (h, ts) -> build (closure (succ h) (slash $ Var T)) ts

-- | (FVarLift) rule productions extracted
--   out of the given production.
fvarLiftR :: Term -> Maybe [Term]
fvarLiftR p = do
    let template = zero
    xs <- matchings template p
    return $ map genFVarLiftR xs

genFVarLiftR :: Term -> Term
genFVarLiftR x =
    case decompose x of
      (Fun Zero [], ts) -> build (closure zero (lift $ Var S)) ts
      _ -> error "Invalid term in FVarLift"

-- | (RVarLift) rule productions extracted
--   out of the given production.
rvarLiftR :: Term -> Maybe [Term]
rvarLiftR p = do
    let template = closure (closure (Var N) (Var S)) shift
    xs <- matchings template p
    return $ map genRVarLiftR xs

genRVarLiftR :: Term -> Term
genRVarLiftR x =
    case decompose x of
      (h, s : Fun Shift [] : ts) ->
          build (closure (succ h) (lift s)) ts
      _ -> error "Invalid term in RVarLift"

-- | (VarShift) rule productions extracted
--   out of the given production.
varShiftR :: Term -> Maybe [Term]
varShiftR p = do
    let template = succ (Var N)
    xs <- matchings template p
    return $ map genVarShiftR xs

genVarShiftR :: Term -> Term
genVarShiftR x =
    case decompose x of
      (Fun Succ [n], ts) ->
          build (closure n shift) ts
      _ -> error "Invalid term in VarShift"
