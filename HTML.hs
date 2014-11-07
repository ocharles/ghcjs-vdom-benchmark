{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module HTML
  ( html, head, title, base, link, meta, style, script, noscript, body, section, nav, article, aside, h1, h2, h3, h4, h5, h6, hgroup, header, footer, address, p, hr, pre, blockquote, ol, ul, li, dl, dt, dd, figure, figcaption, div, a, em, strong, small, s, cite, q, dfn, abbr, data_, time, code, var, samp, kbd, sub, sup, i, b, u, mark, ruby, rt, rp, bdi, bdo, span, br, wbr, ins, del, img, iframe, embed, object, param, video, audio, source, track, canvas, map, area, table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th, form, fieldset, legend, label, input, button, select, datalist, optgroup, option, textarea, keygen, output, progress, meter, details, summary, command, menu, dialog

  , HTML
  , (</>)
  , attrs
  , classes
  , text

  , EventHandlers(..)
  , events
  , onClick
  , renderTo
  , newTopLevelContainer
  , noEvents
  )
where

import Prelude hiding (div, head, map, mapM, sequence, span)
import Data.String (IsString(..))
import Data.HashMap.Strict (HashMap, traverseWithKey)
import Control.Lens hiding (aside, children, pre)
import Data.Text (Text)
import Data.Foldable (for_)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.IORef
import Data.Traversable
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.Foreign
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Text as T
import System.IO.Unsafe
import GHCJS.Marshal
import Immutable

import HTML2

html, head, title, base, link, meta, style, script, noscript, body, section, nav, article, aside, h1, h2, h3, h4, h5, h6, hgroup, header, footer, address, p, hr, pre, blockquote, ol, ul, li, dl, dt, dd, figure, figcaption, div, a, em, strong, small, s, cite, q, dfn, abbr, data_, time, code, var, samp, kbd, sub, sup, i, b, u, mark, ruby, rt, rp, bdi, bdo, span, br, wbr, ins, del, img, iframe, embed, object, param, video, audio, source, track, canvas, map, area, table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th, form, fieldset, legend, label, input, button, select, datalist, optgroup, option, textarea, keygen, output, progress, meter, details, summary, command, menu, dialog :: HTML

html = emptyElement "html"
head = emptyElement "head"
title = emptyElement "title"
base = emptyElement "base"
link = emptyElement "link"
meta = emptyElement "meta"
style = emptyElement "style"
script = emptyElement "script"
noscript = emptyElement "noscript"
body = emptyElement "body"
section = emptyElement "section"
nav = emptyElement "nav"
article = emptyElement "article"
aside = emptyElement "aside"
h1 = emptyElement "h1"
h2 = emptyElement "h2"
h3 = emptyElement "h3"
h4 = emptyElement "h4"
h5 = emptyElement "h5"
h6 = emptyElement "h6"
hgroup = emptyElement "hgroup"
header = emptyElement "header"
footer = emptyElement "footer"
address = emptyElement "address"
p = emptyElement "p"
hr = emptyElement "hr"
pre = emptyElement "pre"
blockquote = emptyElement "blockquote"
ol = emptyElement "ol"
ul = emptyElement "ul"
li = emptyElement "li"
dl = emptyElement "dl"
dt = emptyElement "dt"
dd = emptyElement "dd"
figure = emptyElement "figure"
figcaption = emptyElement "figcaption"
div = emptyElement "div"
a = emptyElement "a"
em = emptyElement "em"
strong = emptyElement "strong"
small = emptyElement "small"
s = emptyElement "s"
cite = emptyElement "cite"
q = emptyElement "q"
dfn = emptyElement "dfn"
abbr = emptyElement "abbr"
data_ = emptyElement "data"
time = emptyElement "time"
code = emptyElement "code"
var = emptyElement "var"
samp = emptyElement "samp"
kbd = emptyElement "kbd"
sub = emptyElement "sub"
sup = emptyElement "sup"
i = emptyElement "i"
b = emptyElement "b"
u = emptyElement "u"
mark = emptyElement "mark"
ruby = emptyElement "ruby"
rt = emptyElement "rt"
rp = emptyElement "rp"
bdi = emptyElement "bdi"
bdo = emptyElement "bdo"
span = emptyElement "span"
br = emptyElement "br"
wbr = emptyElement "wbr"
ins = emptyElement "ins"
del = emptyElement "del"
img = emptyElement "img"
iframe = emptyElement "iframe"
embed = emptyElement "embed"
object = emptyElement "object"
param = emptyElement "param"
video = emptyElement "video"
audio = emptyElement "audio"
source = emptyElement "source"
track = emptyElement "track"
canvas = emptyElement "canvas"
map = emptyElement "map"
area = emptyElement "area"
table = emptyElement "table"
caption = emptyElement "caption"
colgroup = emptyElement "colgroup"
col = emptyElement "col"
tbody = emptyElement "tbody"
thead = emptyElement "thead"
tfoot = emptyElement "tfoot"
tr = emptyElement "tr"
td = emptyElement "td"
th = emptyElement "th"
form = emptyElement "form"
fieldset = emptyElement "fieldset"
legend = emptyElement "legend"
label = emptyElement "label"
input = emptyElement "input"
button = emptyElement "button"
select = emptyElement "select"
datalist = emptyElement "datalist"
optgroup = emptyElement "optgroup"
option = emptyElement "option"
textarea = emptyElement "textarea"
keygen = emptyElement "keygen"
output = emptyElement "output"
progress = emptyElement "progress"
meter = emptyElement "meter"
details = emptyElement "details"
summary = emptyElement "summary"
command = emptyElement "command"
menu = emptyElement "menu"
dialog = emptyElement "dialog"



data EventHandlers = EventHandlers
  { ehClick :: Maybe (Handler ())}

noEvents :: EventHandlers
noEvents = EventHandlers Nothing

events :: Traversal' HTML EventHandlers
events f n = pure n
-- events f (Element t attrs_ handlers c) = Element <$> pure t <*> pure attrs_ <*> f handlers <*> pure c
-- events _ e = pure e

onClick' :: Traversal' EventHandlers (Maybe (Handler ()))
onClick' f (EventHandlers x) = EventHandlers <$> f x

onClick :: Traversal' HTML (Maybe (Handler ()))
onClick = events . onClick'

--------------------------------------------------------------------------------
foreign import javascript unsafe
  "console.time($1)" timeStart :: JSString -> IO ()

foreign import javascript unsafe
  "console.timeEnd($1)" timeEnd :: JSString -> IO ()

timing :: (Applicative m, MonadIO m) => JSString -> m a -> m a
timing lbl m = liftIO (timeStart lbl) *> m <* liftIO (timeEnd lbl)

--------------------------------------------------------------------------------
foreign import javascript unsafe
  "vdom($1)"
  createElement :: HTML -> IO Element

data Diff
foreign import javascript unsafe
  "console.time('ffi:diff'); $r = window.virtualDom.diff($1, $2); console.timeEnd('ffi:diff');"
  diff :: HTML -> HTML -> IO (JSRef Diff)

foreign import javascript unsafe
  "window.virtualDom.patch($1, $2)"
  patch :: Element -> JSRef Diff -> IO ()

--------------------------------------------------------------------------------
-- An element in the DOM that we can render virtualdom elements to
data VNodePresentation = VNodePresentation (IORef HTML) Element

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

newTopLevelContainer :: IO VNodePresentation
newTopLevelContainer = do
  emptyObj <- newObj
  -- setProp ("key" :: JSString) ("0" :: JSString) emptyObj
  initialVNode <- return div
  currentVNode <- newIORef initialVNode
  el <- createElement initialVNode
  Just doc <- currentDocument
  Just bodyNode <- documentGetBody doc
  _ <- nodeAppendChild bodyNode (Just el)
  return (VNodePresentation currentVNode el)
