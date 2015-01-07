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

-- File: Main.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Jan  7, 2015

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clay hiding (i, s, id)
import Control.Monad
import Data.Monoid
import Prelude hiding (div, span)
import System.Environment

import qualified Clay.Media        as Media
import qualified Data.Text.Lazy.IO as Text

-- When running the stylesheet we allow the generation of compacted CSS by
-- using 'compact' as the first argument. Otherwise we dump the default
-- pretty printed version.

main :: IO ()
main =
  do args <- getArgs
     case args of
       "compact" : _
          -> Text.putStr (renderWith compact [] theStylesheet)
       _  -> putCss theStylesheet

---------------------------------------------------------------------------

theStylesheet :: Css
theStylesheet =
  do -- Overall site-wide styling rules.
    freeMonoFontFace
    body ? do
      backgroundColor "#efe"
      textFont
    theHeadings
    (ul <> ol) ? paddingLeft (em 1)
    hr ? marginBottom (em 0.5)
    --sup ? verticalAlign (VerticalAlignValue "super")
    sup ? do
      "vertical-align" -: "super"
    sub ? verticalAlign vAlignSub

theHeadings :: Css
theHeadings = do
  (h1 <> h2 <> h3 <> h4 <> h5 <> h6) ? fontFamily ["FreeMono"] [monospace]
  h1 ? fontSize (em 2.5)
  h2 ? fontSize (em 2)
  h3 ? fontSize (em 1.75)
  h4 ? fontSize (em 1.5)
  h5 ? fontSize (em 1.25)
  h6 ? fontSize (em 1)

freeMonoFontFace :: Css
freeMonoFontFace = do
  fontFace $ do
    fontFamily ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMono.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMono.woff" (Just WOFF)]
  fontFace $ do
    fontFamily ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMonoBold.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMonoBold.woff" (Just WOFF)]
    fontWeight bold
  fontFace $ do
    fontFamily ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMonoOblique.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMonoOblique.woff" (Just WOFF)]
    fontStyle oblique
  fontFace $ do
    fontFamily ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMonoBoldOblique.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMonoBoldOblique.woff" (Just WOFF)]
    fontWeight bold
    fontStyle oblique

textFont :: Css
textFont = do
  fontSize      (px 14)
  lineHeight    (px 21)
  fontFamily    ["FreeMono"] [monospace]
  textRendering optimizeLegibility
