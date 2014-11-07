{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (div)

import Control.Applicative
import Data.Function
import Data.List
import Data.Time
import GHCJS.Foreign
import HTML
import System.Locale

import qualified Data.Text as T

data Class = Class
  { classStartTime :: UTCTime
  , classEndTime :: UTCTime}
  deriving (Show)

main :: IO ()
main = do
  c <- newTopLevelContainer
  renderTo c $
    mkUI $ fmap mkClass [0 .. 3000 :: Int]

  where mkClass n =
         Class { classStartTime = (fromIntegral $ n * 60 * 60) `addUTCTime` UTCTime (fromGregorian 2014 11 7) 0
               , classEndTime = (fromIntegral $ (n + 1) * 60 * 60) `addUTCTime` UTCTime (fromGregorian 2014 11 7) 0}

mkUI :: [Class] -> HTML
mkUI schedule = div </> dayRow <$> groupBy ((==) `on` (utctDay . classStartTime)) schedule

  where
  dayRow day =
    let dayHeader =
          row </> [
           div </> [
             text (toJSString $ T.pack (formatTime defaultTimeLocale "%A %e %B" (classStartTime (Prelude.head day))))]]

        clickableClassRow c =
          row </> [ classRow c ]
    in containerFluid </> (dayHeader : (clickableClassRow <$> day))

  classRow c =
    div </> [
     container </> [
      row </> [
       div </> [ text $ toJSString $ T.pack (formatTime defaultTimeLocale "%H:%M" (classStartTime c))]]]]
