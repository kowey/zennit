{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Arrow        (first)
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

import           Data.Csv

import           Web.Zennit

import Debug.Trace

data Item = Item
    { pUser  :: T.Text
    , pType  :: ItemType
    , pLink  :: T.Text
    , pScore :: Int
    }
  deriving (Show, Eq, Ord)

data ItemType = Comment | Submission
  deriving (Show, Eq, Ord)

instance FromField ItemType where
    parseField s
        | s == "comment"    = pure Comment
        | s == "submission" = pure Submission
        | otherwise         = mzero

instance FromRecord Item where
    parseRecord v
        | V.length v == 4 = Item
               <$> v .!! 0 -- user
               <*> v .!! 1 -- type
               <*> v .!! 2 -- link
               <*> v .!! 3 -- score
        | otherwise     = mzero
      where
        v .!! idx = parseField (BC.dropWhile isSpace (v V.! idx))

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

isComment    p = pType p == Comment
isSubmission p = pType p == Submission

byPost :: [Item] -> [(Maybe Item, [Item])]
byPost items =
    map (first getSubmission) $
    buckets post items
  where
    (comments, submissions) = partition isComment items
    smap = Map.fromList [ (pLink s, s) | s <- submissions ]
    post     = T.pack . takeDirectory . T.unpack . pLink
    getSubmission k  = Map.lookup (k <> "/") smap -- may not be known

about :: V.Vector Item -> T.Text
about res = T.unlines $
    [ "Comments"
    , "--------"
    , commentHistogram pUser comments
    , ""
    , "Posts"
    , "-----"
    , "Number of posts:     " <> tshow totalPosts
    , "…where ewk comments: " <> outOf numCommentedEwk totalPosts
    , "…by ewk:             " <> outOf numByEwk        totalPosts
    ]
  where
    tshow     = T.pack . show
    outOf l t =
        tshow l <> " " <> showPercent (fromIntegral l % fromIntegral t)
    comments = V.filter isComment res
    posts    = byPost (V.toList res)
    byEwk i  = pUser i == "ewk"
    hasEwk   = any byEwk . snd
    totalPosts      = length posts
    numCommentedEwk = length $ filter (any byEwk . snd)         posts
    numByEwk        = length $ filter (maybe False byEwk . fst) posts

commentHistogram f v = T.unlines $
    showPercentages (percentages total core) :
    map (broadbrush total) rest
  where
    broadbrush t (n,h) =
        T.intercalate "\t" [ k, T.pack (show v), showPercent pcent ]
      where
        pcent = fromIntegral v % fromIntegral t
        k     = "… at least " <> T.pack (show n) <> " comments"
        v     = sum (Map.elems h)
    hgram        = histogram . V.toList $ V.map f v
    (core, rest) =
        case walk [100, 50, 10, 2] hgram of
            []     -> error "No comments?!"
            (x:xs) -> (snd x, xs)
    total = V.length v
    -- gah, I recognise this is some kind of fold, but brain not working
    walk [] m     = [(0,m)]
    walk (b:bs) m =
        (b, wanted) : walk bs rest
      where
        (wanted, rest) = Map.partition (> b) m

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

main = do
    [f] <- getArgs
    mp  <- decode HasHeader <$> BL.readFile f
    case mp of
        Left err  -> fail err
        Right res -> T.putStrLn $ about res
