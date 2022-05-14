module Exercises2 where
import Extralib
import Reader
import Term

{- return the list of free variables in the given
   Expr.  The input list of variables is a list of
   variables that this call to freeVars should consider
   bound.  So if you encounter a variable in that list,
   it is not free at that point.  You can add a variable
   to that list to mark it as bound in a recursive call.

   The returned list may contain duplicates.  These are removed
   below.
-}
freeVarsh :: [Var] -> Term -> [Var]
freeVarsh vs (Var v) = (v:vs)
freeVarsh vs (App t1 t2) = (freeVarsh vs t1) ++ (freeVarsh vs t2)
freeVarsh vs (Lam v t) = filter (/= v) ((v:vs) ++ (freeVarsh (v:vs) t))


freeVars :: Term -> [Var]
freeVars t = canonOrd (freeVarsh [] t)

-- rewrite freeVarsh using the ListReader monad (Reader.hs)
freeVarsh' :: Term -> ListReader Var [Var]
freeVarsh' (Var v) = do
  b <- lrElem v
  return (if (b == True) then [] else (pure v))
  
  --return ([] if (b == True) else (pure [v]))

  --return | (b == True) = []
  --       | otherwise = pure [v]

freeVarsh' (App t1 t2) = pure (++) <*> (freeVarsh' t1) <*> (freeVarsh' t2)
freeVarsh' (Lam v t) = lrAdd v (freeVarsh' t)

-- use freeVarsh' and canonOrd to get a list of free variables without duplicates
freeVars' :: Term -> [Var]
freeVars' t = canonOrd (runReader [] (freeVarsh' t))

{- given variables x, y, and z, check to see if z is either x or y.  If it is
   return the other one (so if z is x, return y; if z is y, return x).  If
   z is neither x nor y, return z. -}
swapVar :: Var -> Var -> Var -> Var
swapVar x y z | (x == z) = y
              | (y == z) = x
              | otherwise = z

-- using swapVar, change x into y and vice versa everywhere in the Term,
-- including at binding occurrences (first argument to Lam). 
swapNames :: Var -> Var -> Term -> Term
swapNames x y (Var v) = Var (swapVar x y v)
swapNames x y (App t1 t2) = App (swapNames x y t1) (swapNames x y t2)
swapNames x y (Lam v t) = Lam (swapVar x y v) (swapNames x y t)

{- findNewName vs v

   Given list of variables vs (presumed to be finite),
   find a new version of v that is not in vs, by adding
   single quotation marks ' to the end of v.  If v itself
   is not in vs already, then just return it (no ' marks added) -}
findNewName :: [Var] -> Var -> Var
findNewName [] v = v
findNewName vs v | ((head vs) == v) = (v ++ "'")
                 | otherwise = findNewName (tail vs) v

{- using findNewName and swapNames, safely rename all bound
   variables in the given Term to new names that are
   not in the given list of variables.

   The given list of variables may be assumed to
   contain all free variables of the given Term, and you
   should maintain that property when you recurse, so that
   new names you pick will not conflict with names that are
   bound deeper in the term. -}
renameAwayFrom :: [Var] -> Term -> Term
renameAwayFrom vs (Var v) = Var (findNewName vs v)
renameAwayFrom vs (App t1 t2) = App (renameAwayFrom vs t1) (renameAwayFrom vs t2)
renameAwayFrom vs (Lam v t) = Lam newV (renameAwayFrom (newV:vs) t)
                                where newV = findNewName vs v