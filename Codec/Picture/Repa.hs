{-# LANGUAGE EmptyDataDecls, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
module Codec.Picture.Repa
       ( -- * Primitive types and operations
         Img, imgData
       , convertImage
       -- * Generic interface
       , readImage, decodeImage
       -- * Monomorphic image decoding functions
       , readImageRGBA, readImageRGB, readImageR, readImageG, readImageB
       , decodeImageRGBA, decodeImageRGB, decodeImageR, decodeImageG, decodeImageB
       -- * Image Representations (Phantom Types)
       , RGBA, RGB, R, G, B
       -- * Helper Functions (useful for OpenGL etc.) 
       , toVector
       , toForeignPtr
       , reverseColorChannel
       , flipHorizontally, flipVertically
       -- * Internal Functionallity (exported for advanced uses)
       , ToRGBAChannels(..)
       ) where
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.), Array, (:.)(..), Z(..), DIM3, backpermute, extent)
import qualified Codec.Picture as P
import Codec.Picture hiding (readImage, decodeImage)
import Codec.Picture.Types hiding (convertImage)
import qualified Data.Vector.Storable as S
import Foreign.ForeignPtr
import Data.Word
import Control.Monad
import Data.ByteString as B
import qualified Data.Vector.Unboxed as VU

-- |An all-red image
data R

-- |An all-green image
data G

-- |An all-blue image
data B

-- |A 32-bit image with full red, green, blue and alpha channels.
--
-- The image is stored as Height x Width x ColorChannel.
--
-- The color channel is stored in RGBA order.  For the common OpenGL ordering
-- users might want to use 'reverseColorChannel'.
data RGBA

-- |A 24-bit image with red, green and blue channels
data RGB

-- |@Img a@ is an image where the phantom type 'a' indicates the image format
--
-- All images are held in a three dimensional 'repa' array.  If the image
-- format is only two dimensional (ex: R, G, or B) then the shape is @Z :. y :. x :. 1@.
data Img a = Img { imgData :: Array DIM3 Word8 }

-- |By default, the color channel for 'RGBA' indexes 0 -> R, 1 -> G, 2
-- -> B, 3 -> A.  This is the AGBR byte ordering in OpenGL.  For
-- rendering with OpenGL's RGBA PixelFormat be sure to call
-- reverseColorChannel before converting to a Vector (or directly to
-- bytestring via 'repa-bytestring').
reverseColorChannel :: Img a -> Img a
reverseColorChannel (Img r) = Img (R.backpermute e order r)
  where
  e@(Z :. row :. col :. z)  = R.extent r
  order (Z :. r :. c :. z') = Z :. r :. c :. z - z' - 1

readImageRGBA :: FilePath -> IO (Either String (Img RGBA))
readImageRGBA f = do
  x <- P.readImage f
  return (fmap convertImage x)

readImageRGB :: FilePath -> IO (Either String (Img RGB))
readImageRGB f = do
  x <- P.readImage f
  return (fmap convertImage x)

readImageB :: FilePath -> IO (Either String (Img B))
readImageB f = do
  x <- P.readImage f
  return (fmap convertImage x)

readImageG :: FilePath -> IO (Either String (Img G))
readImageG f = do
  x <- P.readImage f
  return (fmap convertImage x)

readImageR :: FilePath -> IO (Either String (Img R))
readImageR f = do
  x <- P.readImage f
  return (fmap convertImage x)

decodeImageRGBA :: ByteString -> Either String (Img RGBA)
decodeImageRGBA = fmap convertImage . P.decodeImage

decodeImageRGB :: ByteString -> Either String (Img RGB)
decodeImageRGB = fmap convertImage . P.decodeImage

decodeImageR :: ByteString -> Either String (Img R)
decodeImageR = fmap convertImage . P.decodeImage

decodeImageG :: ByteString -> Either String (Img G)
decodeImageG = fmap convertImage . P.decodeImage

decodeImageB :: ByteString -> Either String (Img B)
decodeImageB = fmap convertImage . P.decodeImage

class DecodeImage a where
  decodeImage :: ByteString -> Either String (Img a)

instance DecodeImage RGBA where
  decodeImage = decodeImageRGBA
instance DecodeImage RGB where
  decodeImage = decodeImageRGB
instance DecodeImage R where
  decodeImage = decodeImageR
instance DecodeImage G where
  decodeImage = decodeImageG
instance DecodeImage B where
  decodeImage = decodeImageB

readImage :: DecodeImage a => FilePath -> IO (Either String (Img a))
readImage f = liftM decodeImage (B.readFile f)

-- | O(n)  returning (pointer, length, offset)
toForeignPtr :: Img RGBA -> (ForeignPtr Word8, Int, Int)
toForeignPtr = S.unsafeToForeignPtr . S.convert . R.toVector . imgData

-- |Convert an 'Img' to a storable 'Vector', often useful for OpenGL
-- and other C interfaces.  Notice the format of the data depends on
-- the type of the 'Img a'. O(n)
toVector :: Img a -> S.Vector Word8
toVector (Img a) = S.convert (R.toVector a)

-- Helper functions --
getChannel :: Int -> PixelRGBA8 -> Word8
getChannel 0 (PixelRGBA8 r g b a) = r
getChannel 1 (PixelRGBA8 r g b a) = g
getChannel 2 (PixelRGBA8 r g b a) = b
getChannel _ (PixelRGBA8 r g b a) = a

-- |For any of the JuicyPixel pixels, get the RGBA values
getChan :: (ToRGBAChannels p) => Int -> p -> Word8
getChan c = getChannel c . toRGBAChannels

-- |For any of the JuicyPixel images, get a channel of a particular pixel
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

zeroCopyConvert :: Int -> Image a -> Img b
zeroCopyConvert cc (Image w h dat) =
    let (ptr,off,len) = S.unsafeToForeignPtr dat
        sh = Z :. h :. w :. cc
    in if off == 0
       then flipVertically . Img . R.unsafeFromForeignPtr sh   $  ptr
       else flipVertically . Img . R.fromVector sh $ VU.convert $ dat

-- Now we start the instances needing exported

-- |Converts from 'JuicyPixels' type to the repa-based 'Img' type.
class ConvertImage a b where
  -- |Converts from 'JuicyPixels' type (Usually 'Image' or
  -- 'DynamicImage' to the repa-based 'Img' type.
  convertImage :: a -> Img b

instance ConvertImage DynamicImage RGBA where
  convertImage (ImageY8 i) = convertImage i
  convertImage (ImageYA8 i) = convertImage i
  convertImage (ImageRGB8 i) = convertImage i
  convertImage (ImageRGBA8 i) = zeroCopyConvert 4 i
  convertImage (ImageYCbCr8 i) = convertImage i
  
instance ConvertImage DynamicImage RGB where
  convertImage (ImageY8 i) = convertImage i
  convertImage (ImageYA8 i) = convertImage i
  convertImage (ImageRGB8 i) = zeroCopyConvert 3 i
  convertImage (ImageRGBA8 i) = convertImage i
  convertImage (ImageYCbCr8 i) = convertImage i
  
instance ConvertImage DynamicImage R where
  convertImage (ImageY8 i) = convertImage i
  convertImage (ImageYA8 i) = convertImage i
  convertImage (ImageRGB8 i) = convertImage i
  convertImage (ImageRGBA8 i) = convertImage i
  convertImage (ImageYCbCr8 i) = convertImage i
  
instance ConvertImage DynamicImage G where
  convertImage (ImageY8 i) = convertImage i
  convertImage (ImageYA8 i) = convertImage i
  convertImage (ImageRGB8 i) = convertImage i
  convertImage (ImageRGBA8 i) = convertImage i
  convertImage (ImageYCbCr8 i) = convertImage i
  
instance ConvertImage DynamicImage B where
  convertImage (ImageY8 i) = convertImage i
  convertImage (ImageYA8 i) = convertImage i
  convertImage (ImageRGB8 i) = convertImage i
  convertImage (ImageRGBA8 i) = convertImage i
  convertImage (ImageYCbCr8 i) = convertImage i

instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) RGBA where
  convertImage p@(Image w h dat) =
    let z = 4
    in Img $ R.fromFunction (Z :. h :. w :. z) 
                                    (\(Z :. y :. x :. z') -> getPixel x y (z - z' - 1) p)
  
instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) RGB where
  convertImage p@(Image w h dat) =
    let z = 3
    in Img $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z') -> getPixel x y (z' - z -1) p)

instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) R where
  convertImage p@(Image w h dat) =
    let z = 1
    in Img $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z) -> getPixel x y 0 p)
       
instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) G where
  convertImage p@(Image w h dat) =
    let z = 1
    in Img $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z) -> getPixel x y 1 p)
       
instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) B where
  convertImage p@(Image w h dat) =
    let z = 1
    in Img $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z) -> getPixel x y 2 p)

flipVertically :: Img a -> Img a
flipVertically (Img rp) = Img (backpermute e order rp)
 where
 e@(Z :. row :. col :. z) = extent rp
 order (Z :. oldRow :. oldCol :. oldChan) = Z :. row - oldRow - 1 :. oldCol :. oldChan

flipHorizontally :: Img a -> Img b
flipHorizontally (Img rp) = Img (backpermute e order rp)
 where
 e@(Z :. row :. col :. z) = extent rp
 order (Z :. oldRow :. oldCol :. oldChan) = Z :. oldRow :. col - oldCol - 1 :. oldChan
