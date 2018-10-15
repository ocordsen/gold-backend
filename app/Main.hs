{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Network.Wreq
import Text.CSV
import Data.ByteString.Lazy.Char8

url = "http://www.bundesbank.de/cae/servlet/StatisticDownload?tsId=BBEX3.D.XAU.EUR.EA.AC.C04&its_csvFormat=en&its_fileFormat=csv&mode=its"

main = do
  request <- get url
  parseCSVTest (unpack (request ^. responseBody))
  return ()
