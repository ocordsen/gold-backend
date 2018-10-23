{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 hiding (any, drop, map)
import           Data.Maybe
import           GHC.Generics
import           Network.Wreq
import           Text.CSV
import           Text.Read                  hiding (get)

data Item = Item {date  :: String, value :: Float} deriving (Generic, Show)

instance ToJSON Item where toEncoding = genericToEncoding defaultOptions

main = do
  delete backendUrl
  csvRequest <- get csvUrl
  let lines = drop 3 (unpack (csvRequest ^. responseBody))
  case parseCSV "gold.csv" lines of
    Left error  -> print error
    Right lists -> do
      let payload = encode (catMaybes (map toItem lists))
      let options = defaults & header "Content-Type" .~ ["application/json"]
      postRequest <- postWith options backendUrl payload
      print $ postRequest ^. responseStatus . statusCode
  return ()

toItem :: [String] -> Maybe Item
toItem [x, y, _] = fmap (\value -> Item x value) (readMaybe y :: Maybe Float)
toItem _         = Nothing

backendUrl = "https://gold.cap3.de/gold"

csvUrl = "http://www.bundesbank.de/cae/servlet/StatisticDownload?"
  ++ "tsId=BBEX3.D.XAU.EUR.EA.AC.C04&its_csvFormat=en&its_fileFormat=csv&mode=its"
