{-# LANGUAGE OverloadedStrings #-}
module Web.Zennit where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad        (mzero)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BC
import           Data.Char            (isSpace)
import           Data.Function        (on)
import           Data.List
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Ratio
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Vector          as V
import           Numeric
import           System.Environment
import           System.FilePath


histogram :: Ord a => [a] -> Map.Map a Int
histogram xs = Map.fromListWith (+) $ zip xs (repeat 1)

showPercentages :: Map.Map T.Text (Int, Rational) -> T.Text
showPercentages = T.intercalate "\n"
                . map showPair
                . sortBy (flip compare `on` snd)
                . Map.toList
  where
    showPair (k,(v,p)) =
        T.intercalate "\t" [ k, T.pack (show v), showPercent p ]

showPercent :: Rational -> T.Text
showPercent r =
    T.pack $ showFFloat (Just 1) (100 * fromRational r) "%"

percentages :: Int -> Map.Map a Int -> Map.Map a (Int, Rational)
percentages total xs =
    Map.map (\x -> (x, (fromIntegral x % fromIntegral total))) xs

-- | Seperate out cases where only one person has given that answer
partitionOther :: (Int -> Bool)
               -> Map.Map T.Text Int
               -> (Map.Map T.Text Int, [T.Text])
partitionOther isCore m =
    (plural, Map.keys singular)
  where
    (plural, singular) = Map.partition isCore m

buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))
