{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Implicit.Export.Additive where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.TextBuilderUtils

import Data.Hashable
import Data.HashMap.Strict
import Prelude hiding (lookup,mapM)
import Control.Monad.State hiding (mapM)
import Data.Sequence
import Data.Traversable (mapM)
import Data.Monoid (mempty)
import Text.Blaze.Internal
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Utf8
import Data.ByteString.Lazy
import Codec.Compression.GZip (compress)

data Point3 = P3 !Float !Float !Float
              deriving(Show,Eq)

instance Hashable Point3 where
    hash (P3 x y z) = hash x `hashWithSalt` y `hashWithSalt` z
    hashWithSalt s (P3 x y z) = s `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z

data Tri = Tri !Int !Int !Int
         deriving(Show,Eq)

type VSet = (HashMap Point3 (Point3,Int), Int, Seq (Point3,Point3))

fromℝ3 :: ℝ3 -> Point3
fromℝ3 (x,y,z) = P3 x y z

deduplicate :: [NormedTriangle] -> ([Tri], Seq (Point3,Point3))
deduplicate = shuffle . flip runState (mempty,0,mempty) . mapM lookupTriangle
    where shuffle (triangles,(_,_,verts)) = (triangles,verts)
          lookupTriangle :: NormedTriangle -> State VSet Tri
          lookupTriangle (a,b,c) = do
            a' <- lookupNormedPoint a
            b' <- lookupNormedPoint b
            c' <- lookupNormedPoint c
            return $! Tri a' b' c'

lookupNormedPoint :: (ℝ3,ℝ3) -> State VSet Int
lookupNormedPoint (p,n) = do
  let point = fromℝ3 p 
  (hash,uni,l) <- get
  case lookup point hash of
    Just (_,i)  -> return i
    Nothing -> do
      let normal = fromℝ3 n
      put $! (insert point (normal,uni) hash, uni+1, l |> (point,normal))
      return uni

parent t x = customParent (textTag t) x

vertices :: Seq (Point3,Point3) -> Markup
vertices = parent "vertices" . fmap (const ()) . mapM vertex
    where vertex ((P3 x y z),(P3 nx ny nz))
              = parent "vertex" $ do
                  parent "coordinates" $ do
                                    parent "x" $ toMarkup x
                                    parent "y" $ toMarkup y
                                    parent "z" $ toMarkup z
                  parent "normal" $ do
                                    parent "nx" $ toMarkup nx
                                    parent "ny" $ toMarkup ny
                                    parent "nz" $ toMarkup nz
       
volume :: [Tri] -> Markup
volume = parent "volume" . mapM_ triangle
    where triangle (Tri a b c) 
              = parent "triangle" $ do
                  parent "v1" $ toMarkup a
                  parent "v2" $ toMarkup b
                  parent "v3" $ toMarkup c

file :: [Tri] -> Seq (Point3,Point3) -> Markup
file tri verts = 
    parent "amf" $ parent "object" $ parent "mesh" $ do
      vertices verts
      volume tri

amf :: [NormedTriangle] -> ByteString
amf = compress . renderMarkup . uncurry file . deduplicate
