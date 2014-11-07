{-# LANGUAGE OverloadedStrings #-}
module Bootstrap where

import Prelude hiding (div)

import Control.Lens
import HTML

row :: HTML
row = div & classes .~ ["row"]

containerFluid :: HTML
containerFluid = div & classes .~ ["container-fluid"]

container :: HTML
container = div & classes .~ ["container"]
