{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Implicit.Export.Additive where

import Graphics.Implicit.Definitions
import Data.Hashable
import Data.HashMap.Strict
import Prelude hiding (lookup,mapM)
import Control.Monad.State
import Data.Monoid
import Text.Blaze.Internal
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Utf8
import Data.ByteString.Lazy.Char8 (ByteString)
import Codec.Compression.GZip (compress)

data Point3 = P3 !Float !Float !Float
              deriving(Show,Eq)

instance Hashable Point3 where
    hash (P3 x y z) = hash x `hashWithSalt` y `hashWithSalt` z
    hashWithSalt s (P3 x y z) = s `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z

data Tri = Tri !Int !Int !Int
         deriving(Show,Eq)

type VSet = (HashMap Point3 Int, Int, Markup)

fromℝ3 :: ℝ3 -> Point3
fromℝ3 (x,y,z) = P3 x y z

deduplicate :: [NormedTriangle] -> ([Tri], (Int,Markup))
deduplicate = shuffle . flip runState (mempty,0,mempty) . mapM lookupTriangle
    where shuffle (triangles,(_,u,verts)) = (triangles,(u,verts))
          lookupTriangle :: NormedTriangle -> State VSet Tri
          lookupTriangle (a,b,c) = do
            a' <- lookupNormedPoint a
            b' <- lookupNormedPoint b
            c' <- lookupNormedPoint c
            return $! Tri a' b' c'

lookupNormedPoint :: (ℝ3,ℝ3) -> State VSet Int
lookupNormedPoint (p,normal) = do
  let point = fromℝ3 p 
  (hash,uni,l) <- get
  case lookup point hash of
    Just i  -> return i
    Nothing -> do
      put $! (insert point uni hash, uni+1, Append (vertex point normal) l)
      return uni

parent t x = customParent (textTag t) x

xt p =  Parent "x" "<x" "</x>" $ Content $ PreEscaped $ String $ show p
yt p =  Parent "y" "<y" "</y>" $ Content $ PreEscaped $ String $ show p
zt p =  Parent "z" "<z" "</z>" $ Content $ PreEscaped $ String $ show p

xnt p =  Parent "nx" "<nx" "</nx>" $ Content $ PreEscaped $ String $ show p
ynt p =  Parent "ny" "<ny" "</ny>" $ Content $ PreEscaped $ String $ show p
znt p =  Parent "nz" "<nz" "</nz>" $ Content $ PreEscaped $ String $ show p

v1t p = Parent "v1" "<v1" "</v1>" $ Content $ PreEscaped $ String $ show p
v2t p = Parent "v2" "<v2" "</v2>" $ Content $ PreEscaped $ String $ show p
v3t p = Parent "v3" "<v3" "</v3>" $ Content $ PreEscaped $ String $ show p

vertices :: Markup -> Markup
vertices = parent "vertices" 

vertex :: Point3 -> ℝ3 -> Markup  
vertex (P3 x y z) (nx,ny,nz)
              = parent "vertex" $ do
                  parent "coordinates" $ do
                                    xt x
                                    yt y
                                    zt z
                  parent "normal" $ do
                                    xnt nx
                                    ynt ny
                                    znt nz        

volume :: [Tri] -> Int -> Markup
volume tr u = parent "volume" $ mapM_ triangle tr
    where triangle (Tri a b c) 
              = parent "triangle" $ do
                  v1t $ u - a
                  v2t $ u - b
                  v3t $ u - c

file :: [Tri] -> (Int,Markup) -> Markup
file tri (u,verts) = 
    parent "amf" $ parent "object" $ parent "mesh" $ do
      vertices verts
      volume tri $ u-1

amf :: [NormedTriangle] -> ByteString
amf = compress . renderMarkup . uncurry file . deduplicate

