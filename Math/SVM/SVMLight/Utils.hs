{-# LANGUAGE OverloadedStrings #-}

-- | This package provides a variety of utilities for parsing and producing
-- SVMlight input files.

module Math.SVM.SVMLight.Utils
    ( -- * Types
      Qid(..)
    , FeatureIdx(..)
    , Point(..)
      -- * Parsing SVMlight files
    , point
    , featureIdx
      -- * Generating SVMlight files
    , renderPoints
    , renderPoint
    ) where

import Data.Monoid
import Data.Foldable (foldMap)
import Data.List (intersperse)
import Control.Applicative

import qualified Data.Map.Strict as M

import Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

newtype Qid = Qid Int
            deriving (Show, Ord, Eq)

-- | A feature identifier
newtype FeatureIdx = FIdx Int
                   deriving (Show, Ord, Eq)

featureIdx :: Parser FeatureIdx
featureIdx = fmap FIdx decimal
{-# INLINE featureIdx #-}

qid :: Parser Qid
qid = Qid <$> ("qid:" *> decimal)
{-# INLINE qid #-}

-- | A sample point (e.g. a line of an SVMlight input file).
data Point = Point { pLabel    :: !Int
                   , pQid      :: !(Maybe Qid)
                   , pFeatures :: !(M.Map FeatureIdx Double)
                   , pComment  :: !(Maybe BS.ByteString)
                   }
           deriving (Show, Ord, Eq)

-- | Parse a sample point
point :: Parser Point
point =  do
    label <- decimal
    skipSpace
    qid <- optional qid
    skipSpace
    features <- feature `sepBy'` char ' '
    skipSpace
    comment <- optional $ do
      char '#'
      BS.pack <$> anyChar `manyTill` endOfLine
    skipSpace
    return $ Point label qid (M.fromList features) comment
  where
    feature = (,) <$> featureIdx <* char ':' <*> double
{-# INLINE point #-}

-- | A @Builder@ containing the given @Point@s
renderPoints :: [Point] -> BSB.Builder
renderPoints pts = mconcat $ intersperse "\n" $ map renderPoint pts
{-# INLINEABLE renderPoints #-}

-- | A @Builder@ containing the given @Point@
renderPoint :: Point -> BSB.Builder
renderPoint pt =
    mconcat $ intersperse " " $ [BSB.intDec (pLabel pt)] ++ qid ++ vs ++ c
  where
    vs = map (\(FIdx i,v)->BSB.intDec i<>":"<>BSB.doubleDec v) $ M.assocs (pFeatures pt)
    c = maybe [] (\c->[" #"<>BSB.byteString c]) (pComment pt)
    qid = maybe [] (\(Qid q)->["qid:"<>BSB.intDec q]) (pQid pt)
{-# INLINEABLE renderPoint #-}

