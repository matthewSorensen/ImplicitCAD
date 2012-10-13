{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Implicit.Export.Additive where

import Graphics.Implicit.Definitions
import Data.HashMap.Strict
import Prelude hiding (lookup)
import Control.Monad.State
import Data.Monoid (mempty)
import Text.Blaze.Internal
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Data.ByteString.Lazy.Char8 (ByteString)
import Codec.Compression.GZip (compress)

type Tri = (Int,Int,Int)
type VSet = (HashMap ℝ3 Int, Int, Markup)

deduplicate :: [NormedTriangle] -> ([Tri],Int,Markup)
deduplicate = shuffle . flip runState (mempty,0,mempty) . mapM lookupTriangle
    where shuffle (triangles,(_,u,verts)) = (triangles,u-1,verts)
          lookupTriangle :: NormedTriangle -> State VSet Tri
          lookupTriangle (a,b,c) = do
            a' <- lookupNormedPoint a
            b' <- lookupNormedPoint b
            c' <- lookupNormedPoint c
            return (a',b',c')

lookupNormedPoint :: (ℝ3,ℝ3) -> State VSet Int
lookupNormedPoint (point,normal) = do
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


-- z y x nx ny nz v1 v2 v3
-- vertex coordinates normal

vertices :: Markup -> Markup
vertices = parent "vertices" 

vertex :: ℝ3 -> ℝ3 -> Markup  
vertex (x,y,z) (nx,ny,nz)
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
    where triangle (a,b,c) 
              = parent "triangle" $ do
                  v1t $ u - a
                  v2t $ u - b
                  v3t $ u - c

file :: ([Tri],Int,Markup) -> Markup
file (tri,u,verts) = 
    parent "amf" $ parent "object" $ parent "mesh" $ do
      vertices verts
      volume tri u

amf :: [NormedTriangle] -> ByteString
amf = compress . renderMarkup . file . deduplicate
