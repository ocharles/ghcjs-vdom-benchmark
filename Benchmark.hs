{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (div)

import Data.Function
import Control.Applicative
import Data.List
import System.Locale
import GHCJS.Foreign
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import HTML hiding (b)

data ClassType = ClassType
  { classTypeName :: Text}
  deriving (Show)

data Class = Class
  { classStartTime :: UTCTime
  , classEndTime :: UTCTime
  , classType :: ClassType
  , classBusiness :: Business}
  deriving (Show)

classDuration :: Class -> NominalDiffTime
classDuration Class{..} = classEndTime `diffUTCTime` classStartTime

data Business = Business
  { businessName :: Text
  , businessPicture :: Maybe Text}
  deriving (Show)

main :: IO ()
main = do
  c <- newTopLevelContainer
  renderTo c $
    mkUI $ fmap mkClass [0 .. 3000]

  where mkClass n =
         Class { classStartTime = (fromIntegral $ n * 60 * 60) `addUTCTime` UTCTime (fromGregorian 2014 11 7) 0
               , classEndTime = (fromIntegral $ (n + 1) * 60 * 60) `addUTCTime` UTCTime (fromGregorian 2014 11 7) 0
               , classType = ClassType "Extreme Haskell Programming"
               , classBusiness = Business "GHCJS Ltd." Nothing}

mkUI :: [Class] -> HTML
mkUI schedule =
  div </> [
   div </> [
    case schedule of
      [] -> emptySchedule
      _ -> div </>
           (dayRow <$> groupBy ((==) `on` (utctDay . classStartTime)) schedule)]]

  where
  emptySchedule =
    div </> [
     "There are no events here right now."]

  dayRow day =
    let dayHeader =
          row </> [
           div </> [
             text (toJSString $ T.pack (formatTime defaultTimeLocale "%A %e %B" (classStartTime (Prelude.head day))))]]

        clickableClassRow c =
          row </> [ classRow c ]
    in containerFluid </> (dayHeader : (clickableClassRow <$> day))

classRow :: Class -> HTML
classRow c =
  let rowCell cellClass t =
        div </> [text t]
  in div </> [
      container </> [
       row </> [
        rowCell "start-time" (toJSString $ T.pack (formatTime defaultTimeLocale "%H:%M" (classStartTime c))),
        rowCell "duration" (toJSString $ T.pack (show (classDuration c)))]],
      container </> [
       row </> [
        rowCell "type" (toJSString $ classTypeName (classType c)),
        div </> [ showBusiness (classBusiness c) ]]]]

  where
  showBusiness b =
    maybe (text (toJSString $ businessName b))
          (\pic -> img)
          (businessPicture b)
