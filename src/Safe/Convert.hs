{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Safe.Convert (
    Convert(..)
  , Lenient(..)
  , convertLenient
  , Convertible
) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word
import Data.Bool (bool)
import GHC.Float (float2Double)
import Numeric.Natural (Natural)
import qualified Codec.Binary.UTF8.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8
import qualified Data.ByteString.UTF8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding

newtype Lenient a = Lenient { getLenient :: a }
  deriving (Eq, Ord, Show)

class Convert a b where
  convert :: a -> b

type Convertible a b = (Convert a b, Convert b a)

convertLenient :: Convert a (Lenient b) => a -> b
convertLenient = getLenient . convert

instance (Convert a1 b1, Convert a2 b2, Bifunctor f) => Convert (f a1 a2) (f b1 b2) where
  convert = bimap convert convert

instance (Convert a1 b1, Convert a2 b2, Convert a3 b3) => Convert (a1, a2, a3) (b1, b2, b3) where
  convert ~(a, b, c) = (convert a, convert b, convert c)

instance (Convert a1 b1, Convert a2 b2, Convert a3 b3, Convert a4 b4) => Convert (a1, a2, a3, a4) (b1, b2, b3, b4) where
  convert ~(a, b, c, d) = (convert a, convert b, convert c, convert d)

instance Convert a b => Convert [a] (Maybe (NonEmpty b)) where
  convert []     = Nothing
  convert (a:as) = Just $ convert a :| fmap convert as

instance Convert a b => Convert (NonEmpty a) (NonEmpty b) where convert = fmap convert
instance Convert a b => Convert (Maybe a) (Maybe b)       where convert = fmap convert
instance Convert a b => Convert (NonEmpty a) [b]          where convert = fmap convert . NonEmpty.toList
instance Convert a b => Convert (Maybe a) [b]             where convert = maybe [] (pure . convert)

instance Convert [Word8]       (Lenient String)    where convert = convert . B.pack
instance Convert [Word8]       (Maybe String)      where convert = convert . B.pack
instance Convert [Word8]       (Lenient Text)      where convert = convert . B.pack
instance Convert [Word8]       (Maybe Text)        where convert = convert . B.pack
instance Convert [Word8]       (Lenient LT.Text)   where convert = convert . B.pack
instance Convert [Word8]       (Maybe LT.Text)     where convert = convert . B.pack
instance Convert LB.ByteString (Lenient String)    where convert = Lenient . Data.ByteString.Lazy.UTF8.toString
instance Convert LB.ByteString (Maybe   String)    where convert = checkString . convertLenient
instance Convert LB.ByteString (Lenient Text)      where convert = Lenient . Data.Text.Encoding.decodeUtf8With lenientDecode . mconcat . LB.toChunks
instance Convert LB.ByteString (Maybe   Text)      where convert = eitherToMaybe . Data.Text.Encoding.decodeUtf8' . mconcat . LB.toChunks
instance Convert LB.ByteString (Lenient LT.Text)   where convert = Lenient . Data.Text.Lazy.Encoding.decodeUtf8With lenientDecode
instance Convert LB.ByteString (Maybe   LT.Text)   where convert = eitherToMaybe . Data.Text.Lazy.Encoding.decodeUtf8'
instance Convert ByteString    (Lenient String)    where convert = Lenient . Data.ByteString.UTF8.toString
instance Convert ByteString    (Maybe   String)    where convert = checkString . convertLenient
instance Convert ByteString    (Lenient Text)      where convert = Lenient . Data.Text.Encoding.decodeUtf8With lenientDecode
instance Convert ByteString    (Maybe   Text)      where convert = eitherToMaybe . Data.Text.Encoding.decodeUtf8'
instance Convert ByteString    (Lenient LT.Text)   where convert = Lenient . Data.Text.Lazy.Encoding.decodeUtf8With lenientDecode . LB.fromChunks . pure
instance Convert ByteString    (Maybe   LT.Text)   where convert = eitherToMaybe . Data.Text.Lazy.Encoding.decodeUtf8' . LB.fromChunks . pure

instance Convert ByteString    ByteString     where convert = id
instance Convert ByteString    LB.ByteString  where convert = LB.fromChunks . pure
instance Convert ByteString    [Word8]        where convert = B.unpack
instance Convert LB.ByteString ByteString     where convert = mconcat . LB.toChunks
instance Convert LB.ByteString LB.ByteString  where convert = id
instance Convert LB.ByteString [Word8]        where convert = LB.unpack
instance Convert LT.Text       ByteString     where convert = mconcat . LB.toChunks . Data.Text.Lazy.Encoding.encodeUtf8
instance Convert LT.Text       LB.ByteString  where convert = Data.Text.Lazy.Encoding.encodeUtf8
instance Convert LT.Text       LT.Text        where convert = id
instance Convert LT.Text       String         where convert = LT.unpack
instance Convert LT.Text       Text           where convert = mconcat . LT.toChunks
instance Convert LT.Text       [Word8]        where convert s = convert (convert s :: ByteString)
instance Convert String        ByteString     where convert = Data.ByteString.UTF8.fromString
instance Convert String        LB.ByteString  where convert = Data.ByteString.Lazy.UTF8.fromString
instance Convert String        LT.Text        where convert = LT.pack
instance Convert String        String         where convert = id
instance Convert String        Text           where convert = T.pack
instance Convert String        [Word8]        where convert = Codec.Binary.UTF8.String.encode
instance Convert Text          ByteString     where convert = Data.Text.Encoding.encodeUtf8
instance Convert Text          LB.ByteString  where convert = LB.fromChunks . pure . Data.Text.Encoding.encodeUtf8
instance Convert Text          LT.Text        where convert = LT.fromChunks . pure
instance Convert Text          String         where convert = T.unpack
instance Convert Text          Text           where convert = id
instance Convert Text          [Word8]        where convert s = convert (convert s :: ByteString)
instance Convert Word8         ByteString     where convert = B.singleton
instance Convert Word8         LB.ByteString  where convert = LB.singleton
instance Convert [Word8]       ByteString     where convert = B.pack
instance Convert [Word8]       LB.ByteString  where convert = LB.pack
instance Convert [Word8]       [Word8]        where convert = id

instance Convert Char   ByteString    where convert = B.pack . Codec.Binary.UTF8.String.encodeChar
instance Convert Char   Char          where convert = id
instance Convert Char   Int           where convert = enumToEnum
instance Convert Char   Int32         where convert = enumToEnum
instance Convert Char   Int64         where convert = enumToEnum
instance Convert Char   Integer       where convert = enumToEnum
instance Convert Char   LB.ByteString where convert = LB.pack . convert
instance Convert Char   LT.Text       where convert = LT.singleton
instance Convert Char   String        where convert = pure
instance Convert Char   Text          where convert = T.singleton
instance Convert Char   Word          where convert = enumToEnum
instance Convert Char   Word32        where convert = enumToEnum
instance Convert Char   Word64        where convert = enumToEnum
instance Convert Char   [Word8]       where convert = Codec.Binary.UTF8.String.encodeChar
instance Convert Word16 Char          where convert = enumToEnum
instance Convert Word8  Char          where convert = enumToEnum

instance Convert Double   Double where convert = id
instance Convert Float    Double where convert = float2Double
instance Convert Float    Float  where convert = id
instance Convert Int16    Double where convert = fromIntegral
instance Convert Int16    Float  where convert = fromIntegral
instance Convert Int32    Double where convert = fromIntegral
instance Convert Int8     Double where convert = fromIntegral
instance Convert Int8     Float  where convert = fromIntegral
instance Convert Word16   Double where convert = fromIntegral
instance Convert Word16   Float  where convert = fromIntegral
instance Convert Word32   Double where convert = fromIntegral
instance Convert Word8    Double where convert = fromIntegral
instance Convert Word8    Float  where convert = fromIntegral

instance Convert Int      Rational where convert = fromIntegral
instance Convert Int16    Rational where convert = fromIntegral
instance Convert Int32    Rational where convert = fromIntegral
instance Convert Int64    Rational where convert = fromIntegral
instance Convert Int8     Rational where convert = fromIntegral
instance Convert Rational Rational where convert = id
instance Convert Word     Rational where convert = fromIntegral
instance Convert Word16   Rational where convert = fromIntegral
instance Convert Word32   Rational where convert = fromIntegral
instance Convert Word64   Rational where convert = fromIntegral
instance Convert Word8    Rational where convert = fromIntegral
instance Convert Integer  Rational where convert = fromIntegral
instance Convert Natural  Rational where convert = fromIntegral

instance Convert Int     Integer  where convert = fromIntegral
instance Convert Int16   Integer  where convert = fromIntegral
instance Convert Int32   Integer  where convert = fromIntegral
instance Convert Int64   Integer  where convert = fromIntegral
instance Convert Int8    Integer  where convert = fromIntegral
instance Convert Integer Integer  where convert = fromIntegral
instance Convert Word    Integer  where convert = fromIntegral
instance Convert Word16  Integer  where convert = fromIntegral
instance Convert Word32  Integer  where convert = fromIntegral
instance Convert Word64  Integer  where convert = fromIntegral
instance Convert Word8   Integer  where convert = fromIntegral

instance Convert Natural Integer  where convert = fromIntegral
instance Convert Natural Natural  where convert = fromIntegral
instance Convert Word    Natural  where convert = fromIntegral
instance Convert Word16  Natural  where convert = fromIntegral
instance Convert Word32  Natural  where convert = fromIntegral
instance Convert Word64  Natural  where convert = fromIntegral
instance Convert Word8   Natural  where convert = fromIntegral

instance Convert Int    Int    where convert = fromIntegral
instance Convert Int16  Int    where convert = fromIntegral
instance Convert Int16  Int16  where convert = fromIntegral
instance Convert Int16  Int32  where convert = fromIntegral
instance Convert Int16  Int64  where convert = fromIntegral
instance Convert Int32  Int    where convert = fromIntegral
instance Convert Int32  Int32  where convert = fromIntegral
instance Convert Int32  Int64  where convert = fromIntegral
instance Convert Int64  Int64  where convert = fromIntegral
instance Convert Int8   Int    where convert = fromIntegral
instance Convert Int8   Int16  where convert = fromIntegral
instance Convert Int8   Int32  where convert = fromIntegral
instance Convert Int8   Int64  where convert = fromIntegral
instance Convert Int8   Int8   where convert = fromIntegral
instance Convert Word   Word   where convert = fromIntegral
instance Convert Word   Word64 where convert = fromIntegral
instance Convert Word16 Int32  where convert = fromIntegral
instance Convert Word16 Int64  where convert = fromIntegral
instance Convert Word16 Word   where convert = fromIntegral
instance Convert Word16 Word16 where convert = fromIntegral
instance Convert Word16 Word32 where convert = fromIntegral
instance Convert Word16 Word64 where convert = fromIntegral
instance Convert Word32 Int64  where convert = fromIntegral
instance Convert Word32 Word   where convert = fromIntegral
instance Convert Word32 Word32 where convert = fromIntegral
instance Convert Word32 Word64 where convert = fromIntegral
instance Convert Word64 Word64 where convert = fromIntegral
instance Convert Word8  Int    where convert = fromIntegral
instance Convert Word8  Int16  where convert = fromIntegral
instance Convert Word8  Int32  where convert = fromIntegral
instance Convert Word8  Int64  where convert = fromIntegral
instance Convert Word8  Word   where convert = fromIntegral
instance Convert Word8  Word16 where convert = fromIntegral
instance Convert Word8  Word32 where convert = fromIntegral
instance Convert Word8  Word64 where convert = fromIntegral
instance Convert Word8  Word8  where convert = fromIntegral

instance Convert Word   (Maybe Int16)  where convert = fromIntegralMaybe
instance Convert Word   (Maybe Int32)  where convert = fromIntegralMaybe
instance Convert Word   (Maybe Int64)  where convert = fromIntegralMaybe
instance Convert Word   (Maybe Int8)   where convert = fromIntegralMaybe
instance Convert Word   (Maybe Word16) where convert = fromIntegralMaybe
instance Convert Word   (Maybe Word32) where convert = fromIntegralMaybe
instance Convert Word   (Maybe Word8)  where convert = fromIntegralMaybe
instance Convert Word16 (Maybe Int16)  where convert = fromIntegralMaybe
instance Convert Word16 (Maybe Int8)   where convert = fromIntegralMaybe
instance Convert Word16 (Maybe Word8)  where convert = fromIntegralMaybe
instance Convert Word32 (Maybe Word16) where convert = fromIntegralMaybe
instance Convert Word32 (Maybe Word8)  where convert = fromIntegralMaybe
instance Convert Word64 (Maybe Word16) where convert = fromIntegralMaybe
instance Convert Word64 (Maybe Word8)  where convert = fromIntegralMaybe

fromIntegralMaybe :: (Integral a, Integral b) => a -> Maybe b
fromIntegralMaybe a = let b = fromIntegral a in bool Nothing (Just b) (fromIntegral b == a)

checkString :: String -> Maybe String
checkString [] = Just []
checkString (c:cs)
  | c == Data.ByteString.UTF8.replacement_char = Nothing
  | otherwise = (c:) <$> checkString cs

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

enumToEnum :: (Enum a, Enum b) => a -> b
enumToEnum = toEnum . fromEnum
