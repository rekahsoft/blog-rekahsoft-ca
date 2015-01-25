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

-- File: PageCommon.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Jan 10, 2015

{-# LANGUAGE OverloadedStrings #-}

module PageComponents
( aPost
, businessCard
, tagPageHeading
, pageComponents
) where

import Clay hiding (i, s, id)
import Data.Monoid
import Prelude hiding (div, span, (**))

import Util

pageComponents :: Css
pageComponents = do
  aPost
  businessCard
  tagPageHeading
  srcCodeBlock

aPost :: Css
aPost = do
  ".post" ? do
    makeBorderBox Nothing Nothing

    header ? do
      marginBottom (em 0.8)
      border solid (px 2) "#eee"
      sym borderRadius (px 3)
      sym padding (em 0.35)
      paddingLeft (px 65)

    ".title" ? do
      fontSize (em 2)
      fontWeight bold
      textDecoration underline
      marginBottom nil
      lineHeight (px 35)

    ".title" # ":before" ? do
      content $ stringContent ""
      display block
      backgroundImage $ url "/images/post-icon.svg"
      backgroundRepeat noRepeat
      backgroundPosition $ positioned (pct 50) (pct 50)
      backgroundSize contain
      height (px 55)
      width (px 65)
      marginLeft (px (-65))
      position absolute

    ".info" ? do
      marginBottom nil
      textIndent $ indent (em 1)
      fontSize (em 0.75)

      ".date" ? fontWeight bold
      ".author" ? fontStyle oblique

    footer ? do
      padding (em 0.75) nil (em 0.25) nil
      borderTop solid (px 1) "#eee"

      ".read-more" ? fontWeight bold
      ".no-teaser" ? do
        display block
        content $ stringContent ""
        height (px 1)

      ".tags" # ":before" ? do
        content $ stringContent ""
        backgroundImage $ url "/images/tag.svg"
        backgroundSize contain
        backgroundRepeat noRepeat
        backgroundPosition $ positioned (pct 50) (pct 50)
        sym2 padding (px 3) (em 1.3)

      ".tags" ? do
        marginBottom nil
        paddingRight (em 1.5)
        float floatRight

  ".post" |> p ? do
    textIndent $ indent (em 1.5)
    textAlign justify

  "#pagination" ? do
    display flex
    -- Unable to specify without falling back to plain css
    -- justifyContent spaceBetween
    "justify-content" -: "space-between"
    makeBorderBox Nothing Nothing

  "#pagination" # ":after" ? do
    content none

businessCard :: Css
businessCard = do
  "#business-card" ? do
    border solid (px 2) black
    sym borderRadius (px 5)
    sym padding (px 10)
    minHeight (px 215)
    maxWidth (px 550)
    sym2 margin nil auto

    ".photo" ? do
      backgroundImage $ url "/images/business-card.png"
      border solid (px 1) black
      sym borderRadius (px 10)
      minHeight (px 215)
      minWidth (px 150)
      float floatLeft
      marginRight (px 10)

    ".info" ? do
      borderTop solid (px 2) black
      overflow hidden
      paddingTop (px 8)
    
      star ? display block
      ".name" ? fontWeight bold

tagPageHeading :: Css
tagPageHeading = do
  "#tag" ? do
    textTransform capitalize
    marginBottom (px 8)

  "#tag" # ":before" ? do
    content $ stringContent ""
    backgroundImage $ url "/images/tagged.svg"
    backgroundSize contain
    backgroundRepeat noRepeat
    backgroundPosition $ positioned (pct 50) (pct 50)
    sym padding (px 30)
    margin nil (em 0.35) nil (em 0.1)

srcCodeBlock :: Css
srcCodeBlock = do
  ".code-term" # ".sourceCode" ? do
    display block
    backgroundColor "#111"
    color white
    sym borderRadius (px 3)
    sym2 padding (em 0.5) nil
    marginBottom (em 0.75)
    overflow auto
    maxHeight (em 50)

  ".code-term" ** pre |> code # ".sourceCode" ? do
    backgroundColor inherit
    borderStyle none

  (table # ".sourceCode") <>
    (tr # ".sourceCode")  <>
    (td # ".lineNumbers") <>
    (td # ".sourceCode")  <>
    (table # ".sourceCode" ** pre) ? do
      sym margin nil
      sym padding nil
      border none nil black
      verticalAlign baseline

  td # ".lineNumbers" ? do
    borderRight solid (px 1) "#AAAAAA"
    textAlign $ alignSide sideRight
    color "#AAAAAA"
    paddingLeft (px 8)
    paddingRight (px 8)

  td # ".sourceCode" ?
    paddingLeft (px 5)

  (pre <> code) # ".sourceCode" ** span # ".kw" ? do
    color "#007020"
    fontWeight bold

  (pre <> code) # ".sourceCode" ** span # ".dt" ?
    color "#902000"

  (pre <> code) # ".sourceCode" ** span # ".dv" ?
    color "#40a070"

  (pre <> code) # ".sourceCode" ** span # ".bn" ?
    color "#40a070"

  (pre <> code) # ".sourceCode" ** span # ".fl" ?
    color "#40a070"

  (pre <> code) # ".sourceCode" ** span # ".ch" ?
    color "#4070a0"

  (pre <> code) # ".sourceCode" ** span # ".st" ?
    color "#4070a0"

  (pre <> code) # ".sourceCode" ** span # ".co" ? do
    color "#60a0b0"
    fontStyle italic

  (pre <> code) # ".sourceCode" ** span # ".al" ? do
    color red
    fontWeight bold

  (pre <> code) # ".sourceCode" ** span # ".fu" ?
    color "#06287e"

  -- (pre <> code) # ".sourceCode" ** span # ".re"

  (pre <> code) # ".sourceCode" ** span # ".er" ? do
    color red
    fontWeight bold
