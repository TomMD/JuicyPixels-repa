import Data.Array.Repa as R
import Graphics.Gloss
import Data.Word
import Codec.Picture.Repa
import Codec.Picture
import System.Environment (getArgs)
import Data.Array.Repa.Repr.ByteString as RB

main = do
  ie <- getArgs >>= readImageRGBA . head
  let i = either error id ie
      mkPic = (\(x,y,pic) -> pic) . repaToPicture True . imgData
  -- display (FullScreen (1280,1024)) white (mkPic i)
  display (FullScreen (1280,1024)) white (mkPic $ flipVertically i)
  display (FullScreen (1280,1024)) white (mkPic $ flipHorizontally i)
  display (FullScreen (1280,1024)) white (mkPic $ flipVertically $ flipHorizontally i)

-- |@repaToPicture cacheMeFlag array@ will convert a 'Repa' RGBA array to a tuple of
-- the number of columns, rows and a bitmap for use with 'Gloss'.
repaToPicture :: Bool -> Array RB.B DIM3 Word8 -> (Int, Int, Picture)
repaToPicture b arr = (col, row, bitmapOfByteString row col (RB.toByteString arr) b)
 where
  (Z :. col :. row :. z) = extent arr
