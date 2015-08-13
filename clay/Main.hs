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
import Data.Monoid
import Prelude hiding (div, span, (**), rem)
import System.Environment

import qualified Clay.Media        as Media
import qualified Data.Text.Lazy.IO as Text

import Util
import Header
import PageComponents

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

    -- Styles for miscellaneous elements (h1, ..., h6, body, ul, ol, li, etc..)
    theElements

    -- Style noscipt error message
    "#noscript-alert" ? do
      -- @include border-box(0, #FCD4D4)
      makeBorderBox (Just nil) (Just "#FCD4D4")
      textAlign $ alignSide sideCenter

    -- Site broken into three parts
    theHeader
    pageContent
    theFooter

    ".border-box" ?
      makeBorderBox Nothing Nothing

    -- Style page components (business card, posts, etc..)
    pageComponents

    -- Styles to make site reactive (@media queries)
    reactiveStyles

freeMonoFontFace :: Css
freeMonoFontFace = do
  fontFace $ do
    fontFamily  ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMono.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMono.woff" (Just WOFF)]
  fontFace $ do
    fontFamily  ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMonoBold.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMonoBold.woff" (Just WOFF)]
    fontWeight  bold
  fontFace $ do
    fontFamily  ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMonoOblique.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMonoOblique.woff" (Just WOFF)]
    fontStyle   oblique
  fontFace $ do
    fontFamily  ["FreeMono"] []
    fontFaceSrc [ FontFaceSrcUrl "/fonts/FreeMonoBoldOblique.ttf" (Just TrueType)
                , FontFaceSrcUrl "/fonts/FreeMonoBoldOblique.woff" (Just WOFF)]
    fontWeight  bold
    fontStyle   oblique

theElements :: Css
theElements = do
  body ? do
    backgroundColor "#efe"
    fontSize      (px 14)
    lineHeight    (px 21)
    fontFamily    ["FreeMono"] [monospace]

  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ?
    fontFamily ["FreeMono"] [monospace]
  h1 ? do
    fontSize (em 2.5)
    marginBottom nil
  h2 ? fontSize (em 2)
  h3 ? fontSize (em 1.75)
  h4 ? fontSize (em 1.5)
  h5 ? fontSize (em 1.25)
  h6 ? fontSize (em 1)

  ul <> ol ?
    paddingLeft (em 1)
  hr ? do
    marginBottom (em 0.65)
    marginTop (em 0.65)

  --sup ? verticalAlign (VerticalAlignValue "super")
  sup ? do
    "vertical-align" -: "super"
  sub ? verticalAlign vAlignSub

pageContent :: Css
pageContent = do
  "#page-content" ? do
    opacity 1
    transition "opacity" (ms 250) easeOut (sec 0)
    marginTop (em 1)
    overflow hidden

    ul |> li ? do
      listStyleType none

    ol |> li ? do
      listStylePosition outside
      marginLeft (px 20)

    (ul <> ol) |> li ? do
      marginBottom (em 0.02)

    (ul <> ol) ** (ul <> ol) ? do
      sym margin (rem 1)

  "#page-content" # ".loading" ?
    opacity 0.35

  "#page-content" # ".loading-done" ? do
    transition "opacity" (sec 1) easeIn (sec 0.5)
    opacity 1

  "#page-content" ** ul |> (li # ":before") ? do
    content $ stringContent "\\2192"
    paddingRight (em 0.5)
    fontSize (em 1.5)

theFooter :: Css
theFooter = do
  "#footer-left" ?
    paddingLeft (em 1)

  "#footer-right" ? do
    textAlign $ alignSide sideRight

reactiveStyles :: Css
reactiveStyles = do
  -- Smaller than standard 960 (devices and browsers)
  -- queryOnly Media.screen [Media.maxWidth (px 959) $ do { ... }

  -- Tablet Portrait size to standard 960 (devices and browsers)
  -- queryOnly Media.screen [Media.minWidth (px 768), Media.maxWidth (px 959)] $ do { ... }

  -- All Mobile Sizes (devices and browser)
  queryOnly Media.screen [Media.maxWidth (px 767)] $ do
    "#footer-left" <> "#footer-right" ? do
      textAlign $ alignSide sideCenter

    "#logo" ?
      height (px 130)

  -- Mobile Landscape Size to Tablet Portrait (devices and browsers)
  queryOnly Media.screen [Media.minWidth (px 480), Media.maxWidth (px 767)] $ do
    "#logo-background" ?
      sym2 padding (em 0.5) nil

    "#nav-menu" ** li ?
      sym2 padding (px 10) (em 1.5)

    "#footer-left" ?
      paddingLeft nil

  -- Mobile Portrait Size to Mobile Landscape Size (devices and browsers)
  queryOnly Media.screen [Media.maxWidth (px 476)] $ do
    "#logo-background" ?
      sym padding nil

    "#nav-menu" ** li ?
      sym2 padding (px 10) (px 5)
