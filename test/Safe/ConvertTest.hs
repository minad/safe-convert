{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Safe.ConvertTest where

import Numeric.IEEE (identicalIEEE)
import GHC.Float (double2Float)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Typeable
import Data.Word
import Numeric.Natural
import Safe.Convert
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Special
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

data P a = P

test_Integral_conversion :: [TestTree]
test_Integral_conversion =
  [ integral (P :: P Int)     (P :: P Int)
  , integral (P :: P Int16)   (P :: P Int)
  , integral (P :: P Int16)   (P :: P Int16)
  , integral (P :: P Int16)   (P :: P Int32)
  , integral (P :: P Int16)   (P :: P Int64)
  , integral (P :: P Int32)   (P :: P Int)
  , integral (P :: P Int32)   (P :: P Int32)
  , integral (P :: P Int32)   (P :: P Int64)
  , integral (P :: P Int64)   (P :: P Int64)
  , integral (P :: P Int8)    (P :: P Int)
  , integral (P :: P Int8)    (P :: P Int16)
  , integral (P :: P Int8)    (P :: P Int32)
  , integral (P :: P Int8)    (P :: P Int64)
  , integral (P :: P Int8)    (P :: P Int8)
  , integral (P :: P Word)    (P :: P Word)
  , integral (P :: P Word)    (P :: P Word64)
  , integral (P :: P Word16)  (P :: P Int32)
  , integral (P :: P Word16)  (P :: P Int64)
  , integral (P :: P Word16)  (P :: P Word)
  , integral (P :: P Word16)  (P :: P Word16)
  , integral (P :: P Word16)  (P :: P Word32)
  , integral (P :: P Word16)  (P :: P Word64)
  , integral (P :: P Word32)  (P :: P Int64)
  , integral (P :: P Word32)  (P :: P Word)
  , integral (P :: P Word32)  (P :: P Word32)
  , integral (P :: P Word32)  (P :: P Word64)
  , integral (P :: P Word64)  (P :: P Word64)
  , integral (P :: P Word8)   (P :: P Int)
  , integral (P :: P Word8)   (P :: P Int16)
  , integral (P :: P Word8)   (P :: P Int32)
  , integral (P :: P Word8)   (P :: P Int64)
  , integral (P :: P Word8)   (P :: P Word)
  , integral (P :: P Word8)   (P :: P Word16)
  , integral (P :: P Word8)   (P :: P Word32)
  , integral (P :: P Word8)   (P :: P Word64)
  , integral (P :: P Word8)   (P :: P Word)
  ]

test_Natural_conversion :: [TestTree]
test_Natural_conversion =
  [ integral (P :: P Natural) (P :: P Integer)
  , integral (P :: P Natural) (P :: P Natural)
  , integral (P :: P Word)    (P :: P Natural)
  , integral (P :: P Word16)  (P :: P Natural)
  , integral (P :: P Word32)  (P :: P Natural)
  , integral (P :: P Word64)  (P :: P Natural)
  , integral (P :: P Word8)   (P :: P Natural)
  ]

test_Integer_conversion :: [TestTree]
test_Integer_conversion =
  [ integral (P :: P Int)     (P :: P Integer)
  , integral (P :: P Int16)   (P :: P Integer)
  , integral (P :: P Int32)   (P :: P Integer)
  , integral (P :: P Int64)   (P :: P Integer)
  , integral (P :: P Int8)    (P :: P Integer)
  , integral (P :: P Integer) (P :: P Integer)
  , integral (P :: P Word)    (P :: P Integer)
  , integral (P :: P Word16)  (P :: P Integer)
  , integral (P :: P Word32)  (P :: P Integer)
  , integral (P :: P Word64)  (P :: P Integer)
  , integral (P :: P Word8)   (P :: P Integer)
  ]

test_Float_conversion :: [TestTree]
test_Float_conversion =
  [ testSpecial "Double <-> Double" $  \(a :: Double) -> convert a `identicalIEEE` a
  , testSpecial "Float <-> Float"   $  \(a :: Float)  -> convert a `identicalIEEE` a
  , testSpecial "Float -> Double"   $  \(a :: Float)  -> double2Float (convert a :: Double) `identicalIEEE` a
  , frac (P :: P Int16)   (P :: P Double)
  , frac (P :: P Int16)   (P :: P Float)
  , frac (P :: P Int32)   (P :: P Double)
  , frac (P :: P Int8)    (P :: P Double)
  , frac (P :: P Int8)    (P :: P Float)
  , frac (P :: P Word16)  (P :: P Double)
  , frac (P :: P Word16)  (P :: P Float)
  , frac (P :: P Word32)  (P :: P Double)
  , frac (P :: P Word8)   (P :: P Double)
  , frac (P :: P Word8)   (P :: P Float)
  ]

test_ByteString_isos :: [TestTree]
test_ByteString_isos =
  [ iso (P :: P ByteString)    (P :: P ByteString)
  , iso (P :: P ByteString)    (P :: P LB.ByteString)
  , iso (P :: P ByteString)    (P :: P [Word8])
  , iso (P :: P LB.ByteString) (P :: P LB.ByteString)
  , iso (P :: P LB.ByteString) (P :: P [Word8])
  , iso (P :: P [Word8])       (P :: P [Word8])
  ]

test_String_isos :: [TestTree]
test_String_isos =
  [ iso (P :: P LT.Text) (P :: P LT.Text)
  , iso (P :: P LT.Text) (P :: P String)
  , iso (P :: P LT.Text) (P :: P Text)
  , iso (P :: P String)  (P :: P String)
  , iso (P :: P String)  (P :: P Text)
  , iso (P :: P Text)    (P :: P Text)
  ]

test_Rational_conversion :: [TestTree]
test_Rational_conversion =
  [ iso (P :: P Rational)     (P :: P Rational)
  , frac (P :: P Natural) (P :: P Rational)
  , frac (P :: P Integer) (P :: P Rational)
  , frac (P :: P Int)     (P :: P Rational)
  , frac (P :: P Int16)   (P :: P Rational)
  , frac (P :: P Int32)   (P :: P Rational)
  , frac (P :: P Int64)   (P :: P Rational)
  , frac (P :: P Int8)    (P :: P Rational)
  , frac (P :: P Word)    (P :: P Rational)
  , frac (P :: P Word16)  (P :: P Rational)
  , frac (P :: P Word32)  (P :: P Rational)
  , frac (P :: P Word64)  (P :: P Rational)
  , frac (P :: P Word8)   (P :: P Rational)
  ]

test_Decoding :: [TestTree]
test_Decoding =
  [ decoding (P :: P LT.Text) (P :: P ByteString)
  , decoding (P :: P LT.Text) (P :: P LB.ByteString)
  , decoding (P :: P LT.Text) (P :: P [Word8])
  , decoding (P :: P String)  (P :: P ByteString)
  , decoding (P :: P String)  (P :: P LB.ByteString)
  , decoding (P :: P String)  (P :: P [Word8])
  , decoding (P :: P Text)    (P :: P ByteString)
  , decoding (P :: P Text)    (P :: P LB.ByteString)
  , decoding (P :: P Text)    (P :: P [Word8])
  ]

test_Weird_decoding :: [TestTree]
test_Weird_decoding =
  [ testSpecial "Word8 -> ByteString"    $ \(a :: Word8) -> B.head (convert a) == a
  , testSpecial "Word8 -> LB.ByteString" $ \(a :: Word8) -> LB.head (convert a) == a
  , testSpecial "Char -> ByteString"     $ \(a :: Char)  -> convert (convert a :: ByteString) == Just [a]
  , testSpecial "Char -> LB.ByteString"  $ \(a :: Char)  -> convert (convert a :: LB.ByteString) == Just [a]
  , testSpecial "Char -> Text"           $ \(a :: Char)  -> T.head (convert a) == a
  , testSpecial "Char -> LT.Text"        $ \(a :: Char)  -> LT.head (convert a) == a
  , testSpecial "Char -> String"         $ \(a :: Char)  -> head (convert a) == a
  , testSpecial "Char -> [Word8]"        $ \(a :: Char)  -> convert (B.pack (convert a)) == Just [a]
  ]

test_Char_conversion :: [TestTree]
test_Char_conversion =
  [ enum (P :: P Char)   (P :: P Char)
  , enum (P :: P Char)   (P :: P Int)
  , enum (P :: P Char)   (P :: P Int32)
  , enum (P :: P Char)   (P :: P Int64)
  , enum (P :: P Char)   (P :: P Integer)
  , enum (P :: P Char)   (P :: P Word)
  , enum (P :: P Char)   (P :: P Word32)
  , enum (P :: P Char)   (P :: P Word64)
--  , enum (P :: P Word16) (P :: P Char)
--  , enum (P :: P Word8)  (P :: P Char)
  ]

enum :: forall a b proxy. (Eq a, Show a, Arbitrary a, SpecialValues a, Convert a b, Enum a, Enum b, Typeable a, Typeable b)
         => proxy a -> proxy b -> TestTree
enum pa pb = testSpecial (testName pa "->" pb) $ \(a :: a) -> toEnum (fromEnum (convert a :: b)) == a

integral :: forall a b proxy. (Eq a, Show a, Arbitrary a, SpecialValues a, Convert a b, Convert a (Maybe b), Integral b, Num a, Typeable a, Typeable b)
         => proxy a -> proxy b -> TestTree
integral pa pb = testGroup (testName pa "->" pb)
  [ testSpecial "exact" $ \(a :: a) -> fromIntegral (convert a :: b) == a
  , testSpecial "maybe" $ \(a :: a) -> fmap fromIntegral (convert a :: Maybe b) == Just a ]

frac :: forall a b proxy. (Eq a, Show a, Arbitrary a, SpecialValues a, Convert a b, RealFrac b, Integral a, Typeable a, Typeable b)
         => proxy a -> proxy b -> TestTree
frac pa pb = testSpecial (testName pa "->" pb) $ \(a :: a) -> truncate (convert a :: b) == a

iso :: forall a b proxy. (Eq a, Show a, Arbitrary a, SpecialValues a, Convertible a b, Typeable a, Typeable b)
         => proxy a -> proxy b -> TestTree
iso pa pb = testSpecial (testName pa "<->" pb) $ \(a :: a) -> convert (convert a :: b) == a

decoding :: forall a b proxy. (Eq a, Show a, Arbitrary a, SpecialValues a, Convert a b, Convert b (Maybe a), Typeable a, Typeable b)
         => proxy a -> proxy b -> TestTree
decoding pa pb = testSpecial (testName pa "<~>" pb) $ \(a :: a) -> convert (convert a :: b) == Just a

typeName :: forall proxy a. Typeable a => proxy a -> String
typeName _ = show $ typeOf (undefined :: a)

testName :: (Typeable a, Typeable b) => proxy a -> String -> proxy b -> String
testName a s b = typeName a ++ " " ++ s ++ " " ++ typeName b

testSpecial :: (Show a, SpecialValues a, Arbitrary a) => TestName -> (a -> Bool) -> TestTree
testSpecial n f = testProperty n $ f . getSpecial
