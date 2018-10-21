{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

import System.Environment
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.ByteString as BS
import Data.FileEmbed
import Data.Char

import Dictionary

generate :: String -- ^ all the available characters
         -> Int -- ^ length of desired string
         -> [(Char, Int)] -- ^ known characters
         -> [BS.ByteString] -- ^ all permutations
generate chars len known =
  let combos = filter (\s -> length s == len) $ L.subsequences chars
      perms  = concatMap L.permutations combos
      pred string = all (\(c, i) -> (string !! i) == c) known
  in L.nub . map stringToBs $ filter pred perms 


-- | Wrapper for @generate@
--
-- >>> generateValid "finally" "f***l"
-- ["final", "flail"]
generateValid :: String -- ^ all the available characters
              -> String -- ^ the second "known" argument
              -> [BS.ByteString]
generateValid chars known =
  let len = length known
      known' = filter (isAlpha . fst) (zip known [0..])
  in L.sort $ filter (flip S.member $english) $ generate chars len known'
