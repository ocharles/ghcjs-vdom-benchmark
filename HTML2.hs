{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module HTML2 (HTML, (</>), attrs, classes, emptyElement, text) where

import Control.Applicative
import Control.Lens
import Data.String
import GHCJS.Foreign
import GHCJS.Types
import Unsafe.Coerce
import System.IO.Unsafe

import qualified Immutable

data VNode

newtype HTML = HTML (JSRef VNode)

foreign import javascript safe
  "console.time('new VText'); $r = new VText($1); console.timeEnd('new VText');" text :: JSString -> HTML

foreign import javascript safe
  "new VNode($1)" emptyElement :: JSString -> HTML

foreign import javascript safe
  "$1 && $1.type == 'VirtualNode'" isVNode :: HTML -> JSBool

foreign import javascript safe
  "console.time('setVNodeChildren'); if ($1.type == 'VirtualNode') { $r = new VNode($1.tagName, $1.properties, $2); } else { $r = $1; } console.timeEnd('setVNodeChildren')" setVNodeChildren :: HTML -> JSArray a -> HTML

attrs :: Traversal' HTML Immutable.Map
attrs f n
  | fromJSBool (isVNode n) = f (vNodeGetAttributes n) <&> vNodeSetAttributes n
  | otherwise = pure n

classes :: Traversal' HTML [String]
classes = attrs . at "class" . anon "" (isEmptyStr . fromJSString) . iso (words . fromJSString) (toJSString . unwords)
  where isEmptyStr = (== ("" :: String))

infixl 1 </>
(</>) :: HTML -> [HTML] -> HTML
n </> xs = setVNodeChildren n (unsafePerformIO (toArray (unsafeCoerce xs)))

instance IsString HTML where
  fromString = text . toJSString

foreign import javascript safe
  "console.time('vNodeGetAttributes'); $r = Immutable.Map($1.properties.attributes); console.timeEnd('vNodeGetAttributes');"
  vNodeGetAttributes :: HTML -> Immutable.Map

foreign import javascript safe
  "console.time('vNodeSetAttributes'); $r = new VNode($1.tagName, {'attributes': $2.toJS()}, $1.children); console.timeEnd('vNodeSetAttributes');"
  vNodeSetAttributes :: HTML -> Immutable.Map -> HTML

foreign import javascript unsafe
  "console.time($1)" timeStart :: JSString -> IO ()

foreign import javascript unsafe
  "console.timeEnd($1)" timeEnd :: JSString -> IO ()

timing :: JSString -> IO a -> IO a
timing lbl m = (timeStart lbl) *> m <* (timeEnd lbl)
