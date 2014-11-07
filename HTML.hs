{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module HTML (HTML, div, img, (</>), row, renderTo, newTopLevelContainer, text, container, containerFluid) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef
import Data.String (IsString(..))
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.Foreign
import GHCJS.Types
import Prelude hiding (div, head, map, mapM, sequence, span)
import System.IO.Unsafe
import Unsafe.Coerce

--------------------------------------------------------------------------------
data VNode
newtype HTML = HTML (JSRef VNode)

instance IsString HTML where
  fromString = text . toJSString

--------------------------------------------------------------------------------
foreign import javascript safe
  "console.time('new VText'); $r = new VText($1); console.timeEnd('new VText');" text :: JSString -> HTML

foreign import javascript safe
  "console.time('new VNode'); $r = new VNode($1); console.timeEnd('new VNode');" emptyElement :: JSString -> HTML

foreign import javascript safe
  "console.time('setVNodeChildren'); if ($1.type == 'VirtualNode') { $r = new VNode($1.tagName, $1.properties, $2); } else { $r = $1; } console.timeEnd('setVNodeChildren')" setVNodeChildren :: HTML -> JSArray a -> HTML

--------------------------------------------------------------------------------
infixl 1 </>
(</>) :: HTML -> [HTML] -> HTML
n </> xs = setVNodeChildren n (unsafePerformIO (toArray (unsafeCoerce xs)))

--------------------------------------------------------------------------------
div, img, row, containerFluid, container :: HTML
div = emptyElement "div"
img = emptyElement "div"
row = div
containerFluid = div
container = div

--------------------------------------------------------------------------------
foreign import javascript unsafe
  "vdom($1)"
  createElement :: HTML -> IO Element

--------------------------------------------------------------------------------
data Diff
foreign import javascript unsafe
  "console.time('ffi:diff'); $r = window.virtualDom.diff($1, $2); console.timeEnd('ffi:diff');"
  diff :: HTML -> HTML -> IO (JSRef Diff)

--------------------------------------------------------------------------------
foreign import javascript unsafe
  "window.virtualDom.patch($1, $2)"
  patch :: Element -> JSRef Diff -> IO ()

--------------------------------------------------------------------------------
-- An element in the DOM that we can render virtualdom elements to
data VNodePresentation = VNodePresentation (IORef HTML) Element

--------------------------------------------------------------------------------
-- Render our internal HTML tree representation into a VNode. We first
-- convert our HTML into a VTree, and then diff this against the container
-- and apply the resulting updates.
renderTo :: VNodePresentation -> HTML -> IO ()
renderTo (VNodePresentation ioref el) !e = timing "renderTo" $ do
  -- timing "seq" $ e `seq` return ()
  oldVnode <- readIORef ioref
  patches <- timing "diff" (diff oldVnode e)
  timing "patch" (patch el patches)
  writeIORef ioref e

--------------------------------------------------------------------------------
newTopLevelContainer :: IO VNodePresentation
newTopLevelContainer = do
  let initialVNode = div
  currentVNode <- newIORef initialVNode
  el <- createElement initialVNode
  Just doc <- currentDocument
  Just bodyNode <- documentGetBody doc
  _ <- nodeAppendChild bodyNode (Just el)
  return (VNodePresentation currentVNode el)

--------------------------------------------------------------------------------
foreign import javascript unsafe
  "console.time($1)" timeStart :: JSString -> IO ()

foreign import javascript unsafe
  "console.timeEnd($1)" timeEnd :: JSString -> IO ()

timing :: (Applicative m, MonadIO m) => JSString -> m a -> m a
timing lbl m = liftIO (timeStart lbl) *> m <* liftIO (timeEnd lbl)
