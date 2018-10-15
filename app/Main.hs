{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 hiding (drop)
import           Network.Wreq
import           Prelude
import           Text.CSV

url = "http://www.bundesbank.de/cae/servlet/StatisticDownload?tsId=BBEX3.D.XAU.EUR.EA.AC.C04&its_csvFormat=en&its_fileFormat=csv&mode=its"

main = do
  request <- get url
  parseCSVTest (drop 3 (unpack (request ^. responseBody)))
  return ()
