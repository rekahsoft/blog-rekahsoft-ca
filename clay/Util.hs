-- (C) Copyright Collin J. Doering 2015
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- File: Util.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Jan  9, 2015

{-# LANGUAGE OverloadedStrings #-}
module Util
( makeBorderBox
) where

import Clay hiding (i, s, id)
import Data.Maybe (fromMaybe)
import Prelude hiding (div, span, (**))

makeBorderBox :: Maybe (Size Abs) -> Maybe Color -> Css
makeBorderBox pad backCol =
  let pad' = fromMaybe (px 8) pad
      backCol' = fromMaybe (rgba 250 250 255 165) backCol
  in do
    backgroundColor backCol'
    border solid (px 1) "#888"
    borderRadius (px 5) (px 5) (px 5) (px 5)
    boxShadow (px 2) (px 5) (px 2) "#888"
    sym padding pad'
    marginBottom (em 1.25)
