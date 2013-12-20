import Data.Array.Repa as R
import Graphics.Gloss
import Data.Word
import Codec.Picture.Repa
import Codec.Picture
import System.Environment (getArgs)
-- import Data.Array.Repa.Repr.ByteString as RB

main = do
  ie <- getArgs >>= readImageRGBA . head
  let i = either error id ie
      mkPic img = let (Z:.c:.r:.z) = extent (imgData img)
                  in bitmapOfByteString r c (toByteString img) True
  -- display (FullScreen (800,600)) white (mkPic i)
  display (FullScreen (800,600)) white (mkPic $ flipVertically `onImg` i)
  -- display (FullScreen (800,600)) white (mkPic $ flipHorizontally`onImg`  i)
  -- display (FullScreen (800,600)) white (mkPic $ (flipVertically . flipHorizontally) `onImg` i)

