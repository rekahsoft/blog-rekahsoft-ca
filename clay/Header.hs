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

-- File: Header.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Jan  9, 2015

{-# LANGUAGE OverloadedStrings #-}

module Header
( logoHeader
, navigation
, theHeader
) where

import Clay hiding (i, s, id)
import Prelude hiding (div, span, (**))

theHeader :: Css
theHeader = do
  logoHeader
  navigation

logoHeader :: Css
logoHeader = do
  "#logo-background" ? do
    -- @include background(linear-gradient(#cef, #cff), $fallback: #cef)
    sym2 padding (em 0.75) nil

  "#logo" ? do
    backgroundImage $ url "/images/logo-banner.svg"
    backgroundRepeat noRepeat
    backgroundPosition $ positioned (pct 50) (pct 50)
    backgroundSize contain
    height (px 175)

navigation :: Css
navigation = do
  "#nav" ? do
    border solid (px 2) black
    borderRightWidth 0
    borderLeftWidth 0
    backgroundImage $ url "/images/diagonal-stripes.png"
    -- @include animation(nav-slide 5s linear infinite)
    -- @include animation-play-state(paused)

  -- "#nav" # ".loading" ?
  --   -- @include animation-play-state(running)

  "#nav-menu" ? do
    textAlign $ alignSide sideCenter
    marginBottom nil

    li ? do
      display inlineBlock
      sym margin nil
      sym2 padding (px 10) (em 3)

      ".menuitem" ? do
        display block
        color "#eee"
        fontSize (em 1.5)
        fontWeight bold
        textDecoration none
        textShadow (px 1) (px 1) nil black
        -- @include transition-property(transform)
        -- @include transition-duration(20ms)

      ".menuitem" # ":hover" ? do
        color "#ddd"
        -- @include transform(scale(1.1))

      -- ".menuitem" # ":active" ? do
      --   -- @include transform(scale(0.95))

    li # ".active" ? do
      ".menuitem" ? do
        color "#fff"
        textShadow (px 2) (px 2) nil black
        -- @include transform(scale(1.2))

      -- ".menuitem" # ":hover" ? do
      --   -- @include transition(none)

      -- ".menuitem" # ":active" ? do
      --   -- @include transition(none)
      --   -- @include transform(scale(1.1))

    a # ".rss-icon" ? do
      display inlineBlock
      position absolute
      backgroundImage $ url "/images/rss-icon.png"
      backgroundSize contain
      sym borderRadius (px 5)
      minHeight (px 25)
      minWidth (px 25)
      sym margin nil
      top (px 7)
      right (px 10)

    -- a # ".rss-icon" # ":hover" ?
    --   -- @include transform(scale(1.1))

    -- a # ".rss-icon" # ":active" ?
    --   -- @include transform(scale(0.9))

statusMessages :: Css
statusMessages = do
  "#status" ? do
    display none
    border solid (px 1) black
    borderTop solid nil black
    borderBottomRightRadius (px 5) (px 5)
    borderBottomLeftRadius (px 5) (px 5)
    backgroundColor $ rgb 146 208 240
    -- boxShadow (px 2) (px 5) (px 5) (px 2) "#888888"
    boxShadow (px 2) (px 5) (px 5) "#888888"
    sym padding (px 8)
    marginBottom (em 1.25)

  "#status" # ".error" ?
    backgroundColor "#FD6F6F"

  "#status" # ".success" ? do
    backgroundColor $ rgb 136 211 136
