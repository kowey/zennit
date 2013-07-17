{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad        (mzero)
import qualified Data.ByteString.Lazy as BL
import           Data.Function        (on)
import           Data.List
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Ratio
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Vector          as V
import           Numeric
import           System.Environment
import           System.FilePath

import           Data.Csv

data Answer = Answer
    { aAge           :: T.Text
    , aMeditate      :: Bool
    , aSangha        :: Bool
    , aLineages      :: [T.Text]
    , aInterest      :: T.Text
    , aInterestCodes :: [T.Text]
    , aImprovements  :: T.Text
    , aImproveCodes  :: [T.Text]
    }
  deriving Show

data YesNo = Yes | No
  deriving Show

yes :: Maybe YesNo -> Bool
yes (Just Yes) = True
yes (Just No)  = False
yes Nothing    = False  -- not sure if it's bad to treat missing answers as no :-/

instance FromField YesNo where
    parseField s
        | s == "Yes"  = pure Yes
        | s == "No"   = pure No
        | otherwise   = mzero

instance FromRecord Answer where
    parseRecord v
        | V.length v == 10 = Answer
               <$> v .! 5 -- age
               <*> (yes <$> v .! 1) -- meditate
               <*> (yes <$> v .! 3) -- sangha
               <*> (parseList <$> v .! 2) -- lineages
               <*> v .! 4 -- interest
               <*> (parseList <$> v .! 7) -- interest codes
               <*> v .! 6 -- improvements
               <*> (parseList <$> v .! 8) -- improvement codes
        | otherwise     = mzero
      where
        parseList = filter (not . T.null) . map T.strip . T.splitOn ","

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

process :: V.Vector Answer -> T.Text
process xs = T.unlines $
    [ header "Raw sanghas"
    , showCounts aLineages xs
    , header "Sanghas (practitioners only)"
    , showCounts aLineages (V.filter aSangha xs)
    , header "Why interested in Zen?"
    , showCounts aInterestCodes xs
    , header "How to improve this Reddit?"
    , showCounts aImproveCodes xs
    ]
  where
    header str = str <> "\n" <> T.replicate (T.length str) "-"

textual :: (Answer -> T.Text) -> V.Vector Answer -> T.Text
textual f = T.intercalate "\n\n" . V.toList . V.map f

showCounts :: (Answer -> [T.Text]) -> V.Vector Answer -> T.Text
showCounts f xs =
    showMap primary <>
    showMap (Map.fromList [("OTHER",other)])
  where
    (primary, other) = partitionOther (count xs)
    --
    count :: V.Vector Answer -> Map.Map T.Text Int
    count = histogram . concatMap f . V.toList
    --
    total :: V.Vector Answer -> Int
    total = V.length . V.filter (not . null . f)
    --
    showMap = T.unlines
            . map showPair
            . sortBy (flip compare `on` snd)
            . Map.toList
            . percentages (total xs)
    showPair (k,(v,p)) =
        T.intercalate "\t" [ k, T.pack (show v), showPercent p ]
    showPercent r =
        T.pack $ showFFloat (Just 1) (100 * fromRational r) ""

percentages :: Int -> Map.Map a Int -> Map.Map a (Int, Rational)
percentages total xs =
    Map.map (\x -> (x, (fromIntegral x % fromIntegral total))) xs


histogram :: Ord a => [a] -> Map.Map a Int
histogram xs = Map.fromListWith (+) $ zip xs (repeat 1)

-- | Seperate out cases where only one person has given that answer
partitionOther :: Map.Map T.Text Int -> (Map.Map T.Text Int, Int)
partitionOther m =
    (plural, Map.size singular)
  where
    (plural, singular) = Map.partition (> 1) m

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

main = do
    [f] <- getArgs
    mp  <- decode True <$> BL.readFile f
    case mp of
        Left err  -> fail err
        Right res -> do
            T.putStrLn . process $ res
            T.writeFile (bname <> "-improve.txt")  $ textual aImprovements res
            T.writeFile (bname <> "-interest.txt") $ textual aInterest res
          where
            bname = dropExtensions f
