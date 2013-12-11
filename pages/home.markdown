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
$endsection$
