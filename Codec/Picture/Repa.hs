{-# LANGUAGE EmptyDataDecls, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
module Codec.Picture.Repa
       ( Img, imgData
       , buildImg
       -- * Helper Functions (useful for GL etc.) 
       , toVector
       , toForeignPtr
       ) where
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.), Array, (:.)(..), Z(..), DIM3)
import Codec.Picture
import Codec.Picture.Types
import qualified Data.Vector.Storable as S
import Foreign.ForeignPtr
import Data.Word

data R
data G
data B
data RGBA
data RGB

data Img a = Img { imgData :: Array DIM3 Word8 }

-- | O(n)  returning (pointer, length, offset)
toForeignPtr :: Img RGBA -> (ForeignPtr Word8, Int, Int)
toForeignPtr = S.unsafeToForeignPtr . S.convert . R.toVector . imgData

-- |Convert an 'Img' to a storable 'Vector', often useful for OpenGL
-- and other C interfaces.  Notice the format of the data depends on
-- the type of the 'Img a'. O(n)
toVector :: Img a -> S.Vector Word8
toVector (Img a) = S.convert (R.toVector a)

-- Helper functions --
getChannel 0 (PixelRGBA8 r g b a) = r
getChannel 1 (PixelRGBA8 r g b a) = g
getChannel 2 (PixelRGBA8 r g b a) = b
getChannel _ (PixelRGBA8 r g b a) = a

getChan :: (ToRGBAChannels p) => Int -> p -> Word8
getChan c = getChannel c . toRGBAChannels

getPixel :: (ToRGBAChannels p, Pixel p) => Int -> Int -> Int -> Image p -> Word8
getPixel x y z p = getChan z (pixelAt p x y)

-- Helper class and instances
class ToRGBAChannels a where
  toRGBAChannels :: a -> PixelRGBA8

instance ToRGBAChannels PixelRGBA8 where
  toRGBAChannels = id

instance ToRGBAChannels PixelYCbCr8 where
  toRGBAChannels = promotePixel . (id :: PixelRGB8 -> PixelRGB8) . convertPixel

instance ToRGBAChannels PixelRGB8 where
  toRGBAChannels = promotePixel

instance ToRGBAChannels PixelYA8 where
  toRGBAChannels = promotePixel
  
instance ToRGBAChannels Pixel8 where
  toRGBAChannels = promotePixel

-- Now we start the instances needing exported

class BuildImg a b where
  buildImg :: Image a -> Img b

instance (ToRGBAChannels a, Pixel a) => BuildImg a RGBA where
  buildImg p@(Image x y dat) =
    let z = 4
    in Img $ R.fromFunction (Z :. x :. y :. z) 
                                    (\(Z :. x :. y :. z) -> getPixel x y z p)