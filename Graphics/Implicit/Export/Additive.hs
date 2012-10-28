{-# LANGUAGE OverloadedStrings #-}
module Graphics.Implicit.Export.Additive where

import Graphics.Implicit.Definitions
import Data.HashMap.Strict hiding (map)
import Prelude hiding (lookup)
import Control.Monad.State
import Data.Monoid (mempty)
import Text.Blaze (toMarkup)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)

import Text.Blaze.Internal (MarkupM (..))

import Text.Blaze.Amf
import qualified Text.Blaze.Amf.Elements as E
import qualified Text.Blaze.Amf.Metadata as M
import qualified Text.Blaze.Amf.Attributes as A

type Tri = (Int,Int,Int)
type VSet = (HashMap ℝ3 Int, Int, Markup)

reverseRunMapM :: [State a b] -> a -> ([b],a)
reverseRunMapM lst st = run [] lst st
    where run acc [] st = (acc,st)
          run acc (x:xs) st = let (x',st') = runState x st
                              in run (x':acc) xs st'

deduplicate :: [NormedTriangle] -> ([Tri],Int,Markup)
deduplicate = shuffle . flip reverseRunMapM (mempty,0,mempty) . map lookupTriangle
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
                    E.x $ toMarkup x
                    E.y $ toMarkup y
                    E.z $ toMarkup z
                  E.normal $ do
                    E.nx $ toMarkup nx
                    E.ny $ toMarkup ny
                    E.nz $ toMarkup nz        

volume :: [Tri] -> Int -> Markup
volume tr u = E.volume $ mapM_ triangle tr
    where triangle (a,b,c) 
              = E.triangle $ do
                  E.v1 $ toMarkup $ u - a
                  E.v2 $ toMarkup $ u - b
                  E.v3 $ toMarkup $ u - c

file :: ([Tri],Int,Markup) -> Markup
file (tri,u,verts) = E.amf A.Millimeter $ do
                       M.cad ("ImplicitCAD" :: Text)
                       E.object $ E.mesh $ do
                                            E.vertices verts
                                            volume tri u

-- We now must kill our compress and yell at the stupidity of the amf spec.
amf :: [NormedTriangle] -> ByteString
amf = renderAsLBS . file . deduplicate
