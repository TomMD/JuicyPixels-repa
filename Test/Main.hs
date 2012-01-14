import Data.Array.Repa as R
import Graphics.Gloss
import Data.Word
import Codec.Picture.Repa
import Codec.Picture
import System.Environment (getArgs)
import Data.Array.Repa.ByteString as RB

main = do
  i <- getArgs >>= readImageRGBA . head
  let (x,y,pic) = repaToPicture True . imgData . either error id $ i
  display (FullScreen (1280,1024)) white pic

-- |@repaToPicture cacheMeFlag array@ will convert a 'Repa' RGBA array to a tuple of
-- the number of columns, rows and a bitmap for use with 'Gloss'.
repaToPicture :: Bool -> Array DIM3 Word8 -> (Int, Int, Picture)
repaToPicture b arr = (col, row, bitmapOfByteString row col (toBS arr) b)
  where
  e@(Z :. col :. row :. chan) = extent arr
  order (Z :. oldCol :. oldRow :. oldChan) = Z :. oldCol :. oldRow :. oldChan 
  toBS = RB.toByteString . backpermute e order

