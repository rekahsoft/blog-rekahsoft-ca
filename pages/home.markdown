---
title: Home
author: Collin J. Doering
date: 2013-10-12
weight: 1
---

$section("aboutMe")$
Welcome to the home of [**Collin Doering**](/contact.html).

Links that are loaded using js need further consideration. Specifically there are two options:

1. Use anchor references (like above) which work but will break the nojs site!
2. To have class="active", rel="address:virtualpath" and href set to the resource in question
   This is what it should look like: <a class="internal" rel="address:/contact.html" href="pages/contact.html">Collin Proper</a>.
$endsection$

$section("tools")$
Tools
=====
-----
This website was proudly made with open source software! Specifically:

- [Hakyll][]
- [Skeleton][]
- [JQuery][]
- [JQuery-address][]
- [Sass][]
- [Haml][]
- [Bourbon][]
- [Inkscape][]
- [Gimp][]
- [Gnu Emacs][]
- [Gnu Free Fonts][]
$endsection$

$section("license")$
Terms of Use
============
------------

This site is _Copyright 2013 © - Collin Doering_ and is distributed under the following terms.

1. All rights reserved on the "#! λ Slang" name, as well as on the
   [banner image](/images/logo-banner.svg), favicons~[1](/images/favicon.ico),
   [2](/images/favicon.png)~ and any other image/artwork that embodies the likeness of "#! λ
   Slang"
2. Not withstanding (1), all images and artwork are licensed under the
   [Creative Commons BY-NC-ND License][]
3. Unless otherwise noted, all "_content_" on [#! λ Slang](/) is licenced under the
   [Creative Commons BY-NC-SA License][]
4. Any "_content_" "_delimited as source-code_" is licensed under the [GPLv3][] unless otherwise
   specified
5. Not withstanding (1), (2), or (3), the [Hakyll][] configuration and all files required to
   build this site are available under the [GPLv3][], [here](/)

Definitions
-----------

1. "_Content_" is any html accesible from [#! λ Slang](/), as well as all the "_markdown sources_" used to produces said html.
2. "_Markdown sources_" refers to all files that are written in the
   [Markdown Language](http://daringfireball.net/projects/markdown/) including those
   that use [Pandoc Extensions](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
3. Content is "_delimited as source-code_" when the item (text) in question is:
    1. Contained within a html element with class sourceCode (html)
    2. Contained within a code block (markdown) or fenced code block (pandoc markdown extension)

See More
--------

For more information see:

- [GPLv3][]
- [Creative Commons Licence BY-NS-SA][]
- [Creative Commons BY-NC-ND License][]
$endsection$

$section$
[Hakyll]: http://jaspervdj.be/hakyll/
[Skeleton]: http://www.getskeleton.com/
[JQuery]: http://jquery.com
[JQuery-address]: https://github.com/asual/jquery-address
[Sass]:http://sass-lang.com/
[Haml]: http://haml.info/
[Bourbon]: http://bourbon.io/
[Inkscape]: http://inkscape.org/
[Gimp]: http://www.gimp.org/
[Gnu Emacs]: http://www.gnu.org/software/emacs/
[Gnu Free Fonts]: http://www.gnu.org/software/freefont/

[GPLv3]: https://www.gnu.org/licenses/gpl.html
[Creative Commons Licence BY-NS-SA]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[Creative Commons BY-NC-ND License]: http://creativecommons.org/licenses/by-nc-nd/4.0/
$endsection$
