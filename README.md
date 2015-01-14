# Source Code for **[#! λ Slang](http://blog.rekahsoft.ca)**

## About

[#! λ Slang](http://blog.rekahsoft.ca) is the personal technical blog of *Collin Doering*,
built using software that [respects our freedoms](https://www.gnu.org/philosophy/free-sw.html).

### Features

* [Hakyll][] generated static site
* [Clay][] used for css pre-processing

### License

Simply put, you're welcome to use the code used to generate this site though there are a few restrictions:

* Any images and artwork that embody the likeness of "#! λ Slang" are not to be distributed or
  used and are strictly copyright
* The content of pages and posts can be used with attribution, providing you aren't making money off of it

Please see the LICENSE file for full details.

## Building

All that is needed to build this site is cabal and a way to fetch required packages (whether
directly via the internet or by using the ```fetch``` argument to cabal).

    $ cabal sandbox init # optional but recommended
    $ cabal configure
    $ cabal install --only-dependencies
    $ cabal build
    $ ./site build  # build site
    $ ./site server # view site at http://localhost:3000

[Hakyll]: http://jaspervdj.be/hakyll/
[Clay]: http://fvisser.nl/clay/
[Skeleton]: http://www.getskeleton.com/
[JQuery]: http://jquery.com
[JQuery-address]: https://github.com/asual/jquery-address
[MathJax]: http://www.mathjax.org/
[Inkscape]: http://inkscape.org/
[Gimp]: http://www.gimp.org/
[Gnu Emacs]: http://www.gnu.org/software/emacs/
[Gnu Free Fonts]: http://www.gnu.org/software/freefont/

[GPLv3]: https://www.gnu.org/licenses/gpl.html
[Creative Commons Licence BY-NS-SA]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[Creative Commons BY-NC-ND License]: http://creativecommons.org/licenses/by-nc-nd/4.0/
