---
title: Fun with Quines
author: Collin J. Doering
date: Feb 21, 2015
description: Self replicating programs
tags: programming
---

Quines are cute.

    >"Yields falsehood when preceded by its quotation" yields falsehood when preceded by its quotation.

See wikipedia on quines: <http://en.wikipedia.org/wiki/Quine_(computing)>

<!--more-->

``` {.haskell .code-term .numberLines}
a = "main = putStrLn $ \"a = \" ++ show a ++ \"\\n\" ++ a"
main = putStrLn $ "a = " ++ show a ++ "\n" ++ a
```

``` {.haskell .code-term .numberLines}
-- (C) Copyright Collin Doering 2013
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

-- File: Quine.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Apr 14, 2013

import Data.List (intercalate)

header = [ "-- (C) Copyright Collin Doering 2013"
         , "-- "
         , "-- This program is free software: you can redistribute it and/or modify"
         , "-- it under the terms of the GNU General Public License as published by"
         , "-- the Free Software Foundation, either version 3 of the License, or"
         , "-- (at your option) any later version."
         , "-- "
         , "-- This program is distributed in the hope that it will be useful,"
         , "-- but WITHOUT ANY WARRANTY; without even the implied warranty of"
         , "-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
         , "-- GNU General Public License for more details."
         , "-- "
         , "-- You should have received a copy of the GNU General Public License"
         , "-- along with this program.  If not, see <http://www.gnu.org/licenses/>."
         , ""
         , "-- File: haskell-quine.hs"
         , "-- Author: Collin J. Doering <rekahsoft@gmail.com>"
         , "-- Date: Apr 14, 2013"
         , ""
         , "import Data.List (intercalate)"]

dta = "main = putStrLn $ intercalate \"\\n\" header ++ \"\\n\\nheader = \" ++ \"[ \\\"\" ++ intercalate \"\\\"\\n         , \\\"\" header ++ \"\\\"]\" ++ \"\\n\\ndta = \" ++ show dta ++ \"\\n\" ++ dta"
main = putStrLn $ intercalate "\n" header ++ "\n\nheader = " ++ "[ \"" ++ intercalate "\"\n         , \"" header ++ "\"]" ++ "\n\ndta = " ++ show dta ++ "\n" ++ dta
```
