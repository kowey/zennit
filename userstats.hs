{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
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

data Post = Post
    { pUser  :: T.Text
    , pType  :: PostType
    , pLink  :: T.Text
    , pScore :: Int
    }
  deriving Show

data PostType = Comment | Submission
  deriving (Show, Eq)

instance FromField PostType where
    parseField s
        | s == "comment"    = pure Comment
        | s == "submission" = pure Submission
        | otherwise         = mzero

instance FromRecord Post where
    parseRecord v
        | V.length v == 4 = Post
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

isComment p = pType p == Comment

about :: V.Vector Post -> T.Text
about res = T.unlines $
    [ prettyHistogram pUser comments
    ]
  where
    comments       = V.filter isComment res

prettyHistogram f v =
    showPercentages $ percentages total core
  where
    hgram         = histogram . V.toList $ V.map f v
    (core, other) = partitionOther (> 100) hgram
    total         = V.length v

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

main = do
    [f] <- getArgs
    mp  <- decode True <$> BL.readFile f
    case mp of
        Left err  -> fail err
        Right res -> T.putStrLn $ about res
