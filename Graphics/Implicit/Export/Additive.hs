{-# OPTIONS_GHC -funbox-strict-fields #-}
module Graphics.Implicit.Export.Additive where

import Data.Hashable
import Data.HashMap.Strict
import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Data.List (mapAccumL)

data Sp3 = P3 !Float !Float !Float
         deriving(Show,Eq)

instance Hashable Sp3 where
    hash (P3 x y z) = hash x `hashWithSalt` y `hashWithSalt` z
    hashWithSalt s (P3 x y z) = s `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z

data Tri = Tri !Int !Int !Int

type VertexSet = (HashMap Sp3 Int, Int)

lookupPoint :: ℝ3 -> VertexSet -> (VertexSet, Int)
lookupPoint (x,y,z) (h,u) = let p = P3 x y z
                                      in case lookup p h of
                                           Nothing -> ((insert p u h, u+1), u)
                                           Just i -> ((h,u), i)

dedupTri :: VertexSet -> Triangle -> (VertexSet, Tri)
dedupTri s (a,b,c) = let (s',a') = lookupPoint a s
                         (s'',b') = lookupPoint b s'
                         (s''',c') = lookupPoint c s''
                     in (s''',Tri a' b' c')

deduplicate :: [Triangle] -> ([Sp3],[Tri])
deduplicate = undefined $ mapAccumL dedupTri (empty,0)


-- Then we need to do the tricky part - getting an order list of all of the vertexes.
-- So, what's better?
-- * lazily emitting the list in lookupPoint (+ reverse, or using |> for sequences?)
-- * sorting the the VertexSet's toList
-- * building an ordered set based on the VertexSet's toList
-- Honestly, we just have to implement all of them.
