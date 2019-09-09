module Gen where

import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Test.QuickCheck
import Test.QuickCheck.Utf8

newtype Ident = Ident { fromIdent :: Text }
identUtf8BS = BS.concat <$> listOf (oneof [nonStopByte, twoByte, threeByte])
  where nonStopByte = BS.pack . return <$>
          choose (0 :: Word8, 127) `suchThat` (not . isStopChar)
        isStopChar c = c `elem` map c2w [' ', '\n', '\t']

instance Arbitrary Ident where
  arbitrary = Ident . decodeUtf8 <$> identUtf8BS
