{-# OPTIONS_GHC -funbox-strict-fields #-}
module Graphics.Implicit.Export.Additive where

import Data.Hashable
import Data.HashMap.Strict
import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Data.List (mapAccumL)
import Control.Monad.State
import Data.Sequence hiding (empty)
import Control.Monad (mapM)
import Data.Monoid

data Sp3 = P3 !Float !Float !Float
         deriving(Show,Eq)

instance Hashable Sp3 where
    hash (P3 x y z) = hash x `hashWithSalt` y `hashWithSalt` z
    hashWithSalt s (P3 x y z) = s `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z

data Tri = Tri !Int !Int !Int
         deriving(Show,Eq)

type VSet = (HashMap Sp3 Int, Int, Seq Sp3)

lookupPoint :: ℝ3 -> State VSet Int
lookupPoint (x,y,z) = do
  let p = P3 x y z
  (h,u,l) <- get
  case lookup p h of
    Just i  -> return i
    Nothing -> do
      put $! (insert p u h, u+1, l |> p)
      return u

dedupTri :: Triangle -> State VSet Tri
dedupTri (a,b,c) = do
  a' <- lookupPoint a
  b' <- lookupPoint b
  c' <- lookupPoint c
  return $! Tri a' b' c'

deduplicate :: [Triangle] -> (Seq Sp3, [Tri])
deduplicate t = shuffle $ runState (mapM dedupTri t) (mempty,0,mempty)
    where shuffle (l,(_,_,p)) = (p,l)
