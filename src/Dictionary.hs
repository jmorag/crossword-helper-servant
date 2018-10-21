{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Dictionary where

import qualified Data.HashSet as S
import qualified Data.ByteString as BS
import Data.FileEmbed

rawDict :: BS.ByteString
rawDict = $(embedFile "./words_alpha.txt")

english = [| S.fromList . BS.splitWith (`BS.elem` "\r\n") $ rawDict |]
