{-# LANGUAGE OverloadedStrings #-}
module Graphics.Implicit.Export.Additive where

import Graphics.Implicit.Definitions
import Data.HashMap.Strict
import Prelude hiding (lookup)
import Control.Monad.State
import Data.Monoid (mempty)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Data.ByteString.Lazy.Char8 (ByteString)
import Codec.Compression.GZip (compress)
import Text.Blaze.Internal (MarkupM (..))

import qualified Graphics.Implicit.Export.Additive.Elements as E
import Graphics.Implicit.Export.Additive.Elements (Markup)

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

vertex :: ℝ3 -> ℝ3 -> Markup  
vertex (x,y,z) (nx,ny,nz)
              = E.vertex $ do
                  E.coordinates $ do
                    E.x x
                    E.y y
                    E.z z
                  E.normal $ do
                    E.nx nx
                    E.ny ny
                    E.nz nz        

volume :: [Tri] -> Int -> Markup
volume tr u = E.volume $ mapM_ triangle tr
    where triangle (a,b,c) 
              = E.triangle $ do
                  E.v1 $ u - a
                  E.v2 $ u - b
                  E.v3 $ u - c

file :: ([Tri],Int,Markup) -> Markup
file (tri,u,verts) = 
    E.amf $ E.object $ E.mesh $ do
      E.vertices verts
      volume tri u

amf :: [NormedTriangle] -> ByteString
amf = compress . renderMarkup . file . deduplicate
