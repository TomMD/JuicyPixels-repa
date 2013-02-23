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
       , toForeignPtr, toByteString, toUnboxed, Collapsable(..)
       , onImg
       , reverseColorChannel
       , flipHorizontally, flipVertically
       , vConcat, hConcat
       -- * Internal Functionallity (exported for advanced uses)
       , ToRGBAChannels(..)
       ) where
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Unsafe as RU
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa ((:.), Array, (:.)(..), Z(..), DIM3, backpermute, extent)
import qualified Codec.Picture as P
import Codec.Picture hiding (readImage, decodeImage)
import Codec.Picture.Types hiding (convertImage)
import qualified Data.Vector.Storable as S
import Foreign.ForeignPtr
import Data.Word
import Control.Monad
import Data.ByteString as B
import qualified Data.ByteString.Internal as BI
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
data Img a = Img { imgData :: Array F DIM3 Word8 }

-- |@toByteString arr@ converts images to bytestrings, which is often useful
-- for Gloss.
toByteString :: Img a -> B.ByteString
toByteString (Img arr) =
  let fp = RF.toForeignPtr arr
      (Z :. row :. col :. chan) = extent arr
  in BI.fromForeignPtr fp 0 (col * row * chan)

onImg :: (Array F DIM3 Word8 -> Array F DIM3 Word8) -> Img a -> Img a
onImg f (Img a) = Img (f a)

-- |By default, the color channel for 'RGBA' indexes 0 -> R, 1 -> G, 2
-- -> B, 3 -> A.  This is the AGBR byte ordering in OpenGL.  For
-- rendering with OpenGL's RGBA PixelFormat be sure to call
-- reverseColorChannel before converting to a Vector.
reverseColorChannel :: Img a -> Img a
reverseColorChannel (Img r) = Img (R.computeS $ R.backpermute e order r)
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
toForeignPtr r = (RF.toForeignPtr . imgData $ r, row * col * d, 0)
 where
 (Z :. row :. col :. d) = extent (imgData r)

-- | O(n)  Convert to an unboxed vector
toUnboxed :: Img a -> VU.Vector Word8
toUnboxed = R.toUnboxed . R.computeUnboxedS . R.delay . imgData

class Collapsable a t where
  -- | Converts the color channel into a tuple:
  collapseColorChannel :: Img a -> R.Array R.D R.DIM2 t

instance Collapsable RGBA (Word8,Word8,Word8,Word8) where
    collapseColorChannel (Img a) =
        R.traverse a
             (\(Z:.r:.c:._) -> Z:.r:.c)
             (\l idx -> (l (idx:.0)
                        ,l (idx:.1)
                        ,l (idx:.2)
                        ,l (idx:.3)))

instance Collapsable RGBA (Word8,Word8,Word8) where
    collapseColorChannel (Img a) =
        R.traverse a
             (\(Z:.r:.c:._) -> Z:.r:.c)
             (\l idx -> (l (idx:.0)
                        ,l (idx:.1)
                        ,l (idx:.2)))

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

{-
zeroCopyConvert :: Int -> Image a -> Img b
zeroCopyConvert cc (Image w h dat) =
    let (ptr,off,len) = S.unsafeToForeignPtr dat
        sh = Z :. h :. w :. cc
    in if off == 0
       then flipVertically . Img . R.unsafeFromForeignPtr sh   $  ptr
       else flipVertically . Img . R.fromVector sh $ VU.convert $ dat
-}

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
  convertImage (ImageRGBA8 i) = convertImage i -- zeroCopyConvert 4 i
  convertImage (ImageYCbCr8 i) = convertImage i

instance ConvertImage DynamicImage RGB where
  convertImage (ImageY8 i) = convertImage i
  convertImage (ImageYA8 i) = convertImage i
  convertImage (ImageRGB8 i) = convertImage i -- zeroCopyConvert 3 i
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
    in Img $ R.computeS $ R.fromFunction (Z :. h :. w :. z) 
                                    (\(Z :. y :. x :. z') -> getPixel x y (z - z' - 1) p)

instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) RGB where
  convertImage p@(Image w h dat) =
    let z = 3
    in Img $ R.computeS $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z') -> getPixel x y (z' - z -1) p)

instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) R where
  convertImage p@(Image w h dat) =
    let z = 1
    in Img $ R.computeS $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z) -> getPixel x y 0 p)

instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) G where
  convertImage p@(Image w h dat) =
    let z = 1
    in Img $ R.computeS $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z) -> getPixel x y 1 p)

instance (ToRGBAChannels a, Pixel a) => ConvertImage (Image a) B where
  convertImage p@(Image w h dat) =
    let z = 1
    in Img $ R.computeS $ R.fromFunction (Z :. h :. w :. z)
                            (\(Z :. y :. x :. z) -> getPixel x y 2 p)

-- |Flip an image vertically
flipVertically :: Array F DIM3 Word8 -> Array F DIM3 Word8
flipVertically rp = (R.computeS $ backpermute e order rp)
 where
 e@(Z :. row :. col :. z) = extent rp
 order (Z :. oldRow :. oldCol :. oldChan) = Z :. row - oldRow - 1 :. oldCol :. oldChan

-- |Flip an image horizontally
flipHorizontally :: Array F DIM3 Word8 -> Array F DIM3 Word8
flipHorizontally rp = (R.computeS $ backpermute e order rp)
 where
 e@(Z :. row :. col :. z) = extent rp
 order (Z :. oldRow :. oldCol :. oldChan) = Z :. oldRow :. col - oldCol - 1 :. oldChan

-- |Stack the images vertically, placing the first image on top of the second.
vStack :: Array R.D DIM3 Word8 -> Array R.D DIM3 Word8 -> Array R.D DIM3 Word8
vStack a b = R.traverse2 a b combExtent stack
  where
  combExtent (Z :. h1 :. w1 :. d1) (Z :. h2 :. w2 :. d2)
    = Z :. (h1 + h2)  :. min w1 w2 :. min d1 d2
  (Z :. ha :. _ :. _) = R.extent a
  (Z :. hb :. _ :. _) = R.extent b
  stack fa fb (Z :. h :. w :. d)
        | h < hb    = fb (Z :. h :. w :. d)
        | otherwise = fa (Z :. h - hb :. w :. d)

-- |Combines a list of images such that the first image is on top, then
-- the second, and so on.
vConcat :: [Array F DIM3 Word8] -> Array F DIM3 Word8
vConcat [] = error "vConcat: Can not concat an empty list into a Repa array"
vConcat xs = R.computeS $ Prelude.foldl1 vStack (Prelude.map R.delay xs)

-- |Combines a list of images such that the first image is on the left, then
-- the second, and so on.
hConcat :: [Array F DIM3 Word8] -> Array F DIM3 Word8
hConcat [] = error "hConcat: Can not concat an empty list into a Repa array"
hConcat xs = R.computeS $ Prelude.foldl1 hStack (Prelude.map R.delay xs)

-- |Stack the images horozontally, placing the first image on the left of the second.
hStack :: Array R.D DIM3 Word8 -> Array R.D DIM3 Word8 -> Array R.D DIM3 Word8
hStack a b = R.traverse2 a b combExtent stack
  where
  combExtent (Z :. r1 :. c1 :. d1) ( Z :. r2 :. c2 :. d2)
    = Z :. min r1 r2 :. c1 + c2 :. min d1 d2
  (Z :. _ :. ca :. _) = R.extent a
  stack fa fb (Z :. r :. c :. d)
        | c < ca    = fa (Z :. r :. c :. d)
        | otherwise = fb (Z :. r :. c - ca :. d)

-- | A histogram is a one dimensional array where each element
-- indicates how many pixels held the value represented by the element's
-- index.
type Histogram = R.Array R.D R.DIM1 Word8

-- | Compute the RGBA historgrams of the image.
histograms :: Img a -> (Histogram, Histogram, Histogram, Histogram)
histograms (Img arr) =
    let (Z:.nrRow:.nrCol:._) = R.extent arr
        zero = R.fromFunction (Z:.256) (\_ -> 0 :: Word8)
        incElem idx x = RU.unsafeTraverse x id (\l i -> l i + if i==(Z:.fromIntegral idx) then 1 else 0)
    in Prelude.foldl (\(hR, hG, hB, hA) (row,col) ->
             let r = R.unsafeIndex arr (Z:.row:.col:.0)
                 g = R.unsafeIndex arr (Z:.row:.col:.1)
                 b = R.unsafeIndex arr (Z:.row:.col:.2)
                 a = R.unsafeIndex arr (Z:.row:.col:.3)
             in (incElem r hR, incElem g hG, incElem b hB, incElem a hA))
          (zero,zero,zero,zero)
          [ (row,col) | row <- [0..nrRow-1], col <- [0..nrCol-1] ]
