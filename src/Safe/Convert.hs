{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE UndecidableInstances #-}

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
import GHC.TypeLits
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

class Convert a b where
  convert :: a -> b

type Convertible a b = (Convert a b, Convert b a)

type family Lo a :: Nat
type family Hi a :: Nat

type instance Lo Word8 = 0
type instance Hi Word8 = 0xFF
type instance Lo Word16 = 0
type instance Hi Word16 = 0xFFFF
type instance Lo Word32 = 0
type instance Hi Word32 = 0xFFFFFFFF
type instance Lo Word64 = 0
type instance Hi Word64 = 0xFFFFFFFFFFFFFFFF
type instance Lo Word = 0
type instance Hi Word = 0xFFFFFFFFFFFFFFFF
type instance Lo Int8 = 0x80
type instance Hi Int8 = 0x7F
type instance Lo Int16 = 0x8000
type instance Hi Int16 = 0x7FFF
type instance Lo Int32 = 0x80000000
type instance Hi Int32 = 0x7FFFFFFF
type instance Lo Int64 = 0x8000000000000000
type instance Hi Int64 = 0x7FFFFFFFFFFFFFFF
type instance Lo Int = 0x8000000000000000
type instance Hi Int = 0x7FFFFFFFFFFFFFFF
type instance Lo Char = 0
type instance Hi Char = 0x10FFFF
type instance Lo Integer = 0xFFFFFFFFFFFFFFFFF
type instance Hi Integer = 0xFFFFFFFFFFFFFFFFF
type instance Lo Rational = 0xFFFFFFFFFFFFFFFFF
type instance Hi Rational = 0xFFFFFFFFFFFFFFFFF
type instance Lo Natural = 0
type instance Hi Natural = 0xFFFFFFFFFFFFFFFFF

type instance Lo Float = 0xFFFFF
type instance Hi Float = 0xFFFFF
type instance Lo Double = 0xFFFFFFFFF
type instance Hi Double = 0xFFFFFFFFF

type family Or (a :: Bool) (b :: Bool) :: Bool
type instance Or 'True  'True  = 'True
type instance Or 'True  'False = 'True
type instance Or 'False 'True  = 'True
type instance Or 'False 'False = 'False

type family And (a :: Bool) (b :: Bool) :: Bool
type instance And 'True  'True  = 'True
type instance And 'True  'False = 'False
type instance And 'False 'True  = 'False
type instance And 'False 'False = 'False

type family Not (a :: Bool) :: Bool
type instance Not 'True  = 'False
type instance Not 'False = 'True

--type InRange a b = (Lo a <= Lo b, Hi a <= Hi b)
type InRange a b = (And (Lo a <=? Lo b) (Hi a <=? Hi b)) ~ 'True
type OutOfRange a b = (Not (And (Lo a <=? Lo b) (Hi a <=? Hi b))) ~ 'True

instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Word (Maybe a)               where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Word8 (Maybe a)              where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Word16 (Maybe a)             where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Word32 (Maybe a)             where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Word64 (Maybe a)             where convert = fromIntegralMaybe

instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Int (Maybe a)               where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Int8 (Maybe a)              where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Int16 (Maybe a)             where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Int32 (Maybe a)             where convert = fromIntegralMaybe
instance {-# OVERLAPPABLE #-} (OutOfRange Word a, Integral a) => Convert Int64 (Maybe a)             where convert = fromIntegralMaybe

instance {-# OVERLAPPABLE #-} (InRange Word a,   Num a) => Convert Word   a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Word16 a, Num a) => Convert Word16 a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Word32 a, Num a) => Convert Word32 a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Word64 a, Num a) => Convert Word64 a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Word8 a,  Num a) => Convert Word8  a where convert = fromIntegral

instance {-# OVERLAPPABLE #-} (InRange Int a,    Num a) => Convert Int    a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Int16 a,  Num a) => Convert Int16  a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Int32 a,  Num a) => Convert Int32  a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Int64 a,  Num a) => Convert Int64  a where convert = fromIntegral
instance {-# OVERLAPPABLE #-} (InRange Int8 a,   Num a) => Convert Int8   a where convert = fromIntegral

newtype Lenient a = Lenient { getLenient :: a }
  deriving (Eq, Ord, Show)

convertLenient :: Convert a (Lenient b) => a -> b
convertLenient = getLenient . convert

instance (Convert a1 b1, Convert a2 b2, Bifunctor f) => Convert (f a1 a2) (f b1 b2) where
  convert = bimap convert convert

instance (Convert a1 b1, Convert a2 b2, Convert a3 b3) => Convert (a1, a2, a3) (b1, b2, b3) where
  convert ~(a, b, c) = (convert a, convert b, convert c)

instance (Convert a1 b1, Convert a2 b2, Convert a3 b3, Convert a4 b4) => Convert (a1, a2, a3, a4) (b1, b2, b3, b4) where
  convert ~(a, b, c, d) = (convert a, convert b, convert c, convert d)

instance Convert a b => Convert (NonEmpty a) (NonEmpty b) where convert = fmap convert
instance Convert a b => Convert (NonEmpty a) [b]          where convert = fmap convert . NonEmpty.toList

instance Convert a b => Convert [a] (Maybe (NonEmpty b))  where convert = \case [] -> Nothing; (a:as) -> Just $ convert a :| fmap convert as
instance Convert a b => Convert (Maybe a) (Maybe b)       where convert = fmap convert
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
instance Convert [Word8]       ByteString     where convert = B.pack
instance Convert [Word8]       LB.ByteString  where convert = LB.pack
instance Convert [Word8]       [Word8]        where convert = id
instance {-# OVERLAPPABLE #-} Convert Word8         ByteString     where convert = B.singleton
instance {-# OVERLAPPABLE #-} Convert Word8         LB.ByteString  where convert = LB.singleton

instance Convert Char   ByteString    where convert = B.pack . Codec.Binary.UTF8.String.encodeChar
instance Convert Char   Char          where convert = id
instance Convert Char   LB.ByteString where convert = LB.pack . convert
instance Convert Char   LT.Text       where convert = LT.singleton
instance Convert Char   String        where convert = pure
instance Convert Char   Text          where convert = T.singleton
instance Convert Char   Word          where convert = enumToEnum
instance Convert Char   [Word8]       where convert = Codec.Binary.UTF8.String.encodeChar
instance {-# OVERLAPPABLE #-} (OutOfRange Char a, Enum a) => Convert Char (Maybe a) where convert = enumToEnumMaybe
instance {-# OVERLAPPABLE #-} (InRange Char a, Enum a) => Convert Char a         where convert = enumToEnum

instance Convert Double   Double where convert = id
instance Convert Float    Double where convert = float2Double
instance Convert Float    Float  where convert = id

instance Convert Rational Rational where convert = id
instance Convert Integer  Rational where convert = fromIntegral
instance Convert Natural  Rational where convert = fromIntegral

instance Convert Integer Integer where convert = id
instance Integral a => Convert Integer (Maybe a) where convert = Just . fromIntegral

instance Convert Natural (Maybe Natural) where convert = Just
instance Convert Natural Natural where convert = id
instance Convert Natural Integer where convert = fromIntegral

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

enumToEnumMaybe :: (Enum a, Enum b, Eq a) => a -> Maybe b
enumToEnumMaybe a = let b = enumToEnum a in bool Nothing (Just b) (enumToEnum b == a)
