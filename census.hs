{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad        (mzero)
import qualified Data.ByteString.Lazy as BL
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

import           Data.Csv

import           Web.Zennit

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
        parseList = map T.toLower
                  . filter (not . T.null) . map T.strip
                  . T.splitOn ","

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

data Lump = Lump
    { lumpThese :: [T.Text]
    , lumpInto  :: T.Text
    }
  deriving (Show, Eq)

interests :: [Lump]
interests =
    [ Lump [ "literature", "watts", "philosophy" ] "philosophy/media"
    , Lump [ "peace", "suffering"     ] "peace/suffering"
    , Lump [ "koans", "meditation"    ] "koans/meditation"
    , Lump [ "non-dual", "suchness", "mu", "attachment", "ego" ] "non-dual/suchness/mu/attachment/ego"
    , Lump [ "life", "background"     ] "life/background"
    , Lump [ "approach", "simplicity" ] "approach"
    , Lump [ "spirituality", "buddhism" ] "buddhism/spirituality"
    , Lump [ "now"  ]                   "present-moment"
    , Lump [ "self" ]                   "self-improvement"
    , Lump [ "more" ]                   "see-details"
    ]

improvements :: [Lump]
improvements =
    [ Lump [ "more" ]                     "see-details"
    , Lump [ "newbie" ]                   "more-newbie-friendly"
    , Lump [ "infrastructure", "events" ] "events/infrastructure"
    , Lump [ "moderation"               ] "moderation"
    , Lump [ "anti-ewk", "pro-ewk", "experts" ] "participants"
    ]

lump :: [Lump] -> T.Text -> T.Text
lump ls t =
     maybe t lumpInto $
     find (elem t . lumpThese) ls

process :: V.Vector Answer -> T.Text
process xs = T.unlines $
    [ header "Raw sanghas"
    , showCounts aLineages xs
    , header "Sanghas (practitioners only)"
    , showCounts aLineages (V.filter aSangha xs)
    , header "Why interested in Zen?"
    , showCounts interest xs
    , header "How to improve this Reddit?"
    , showCounts improve  xs
    ]
  where
    header str = str <> "\n" <> T.replicate (T.length str) "-"
    interest = map (lump interests)    . aInterestCodes
    improve  = map (lump improvements) . aImproveCodes

textual :: (Answer -> T.Text) -> V.Vector Answer -> T.Text
textual f = T.intercalate "\n\n" . V.toList . V.map f

showCounts :: (Answer -> [T.Text]) -> V.Vector Answer -> T.Text
showCounts f xs =
    showMap primary <>
    showMap (Map.fromList [("OTHER",length other)]) <>
    "\n" <> T.intercalate ", " (map (snippet 10) other) <>
    "\n" <> responseLen
  where
    responseLen = "Num responses:\n" <> T.pack
        (show . Map.toList . histogram $ map (length . f) $ V.toList xs)
    snippet n t =
        if T.length t <= n
             then t
             else T.take n t <> "…"
    (primary, other) = partitionOther (> 1) (count xs)
    --
    count :: V.Vector Answer -> Map.Map T.Text Int
    count = histogram . concatMap f . V.toList
    --
    total :: V.Vector Answer -> Int
    total = V.length . V.filter (not . null . f)
    --
    showMap = (<> "\n")
            . showPercentages
            . percentages (total xs)

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
            T.writeFile (bname <> "-improve.txt")  $ textual aImprovements $
                V.filter (\a -> "more" `elem` aImproveCodes a) res
            T.writeFile (bname <> "-interest.txt") $ textual aInterest $
                V.filter (\a -> "more" `elem` aInterestCodes a) res
            T.writeFile (bname <> "-full-improve.txt")  $
                textual aImprovements res
            T.writeFile (bname <> "-full-interest.txt") $
                textual aInterest res
          where
            bname = dropExtensions f
