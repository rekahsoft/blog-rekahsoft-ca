# Source Code for **[#! Lambda Slang](http://blog.rekahsoft.ca)**

[![build status](https://gitlab.com/rekahsoft/blog-rekahsoft/badges/master/build.svg)](https://gitlab.com/rekahsoft/blog-rekahsoft/commits/master)

* [Features](#features)
* [Tools](#tools)
* [License](#license)
* [Building](#building)
* [Running Tests](#running-tests)
* [Deploying](#deploying)
* [Issues](#issues)
* [Road Map](#road-map)

[#! Lambda Slang](http://blog.rekahsoft.ca) is the personal technical blog of *Collin Doering*,
built using software that [respects our freedoms](https://www.gnu.org/philosophy/free-sw.html).

## Features <a name="features"></a>

* [Single Page Application (SPA)](http://en.wikipedia.org/wiki/Single-page_application)
* Utilizes CSS 3
* Uses HTML5 Application Cache for offline viewing of website

## Tools <a name="tools"></a>

The creation of this website was made possible by the following open source tools and libraries:

* [Hakyll][] is used to generate site from static files
* [Clay][] is used for CSS pre-processing
* [Skeleton][] is used for CSS boilerplate
* [MathJax][] is used for rendering mathematics
* [Inkscape][] and the [Gimp][] were used to create various images/artwork
* [Selenium][] is used for automated testing using real browsers
* [Gnu Free Fonts][], specifically *FreeMono* is used as main font
* [Gnu Emacs][], because there is no place like home; and no greater editor!

## License <a name="license"></a>

Simply put, you're welcome to use the code used to generate this site though there are a few restrictions:

* Any images and artwork that embody the likeness of "#! Lambda Slang" are not to be distributed or
  used and are strictly copyright
* The content of pages and posts can be used with attribution, providing you aren't making money off of it

Various licenses ([GPLv3][], [Creative Commons BY-NC-SA License][], and
[Creative Commons BY-NC-ND License][]) are deployed dependent on which part of the site is in
question. Please see the LICENSE file for full details.

## Building <a name="building"></a>

All that is needed to build this site is cabal and a way to fetch required packages (whether
directly via the internet or by using the ```fetch``` argument to cabal).

    $ cabal sandbox init # optional but recommended
    $ cabal configure
    $ cabal install --only-dependencies
    $ cabal build
    $ ./site build  # build site
    $ ./site server # view site at http://localhost:3000

## Running Tests <a name="running-tests"></a>

To the run the tests that accompany this site, one must have [Selenium][] installed and an
instances running on port 4444. Then one must run the following:

    $ cabal configure --enable-tests
    $ cabal install --enable-tests --only-dependencies
    $ ./site test -s

The `-s` or `--run-selenium` option to `site test` starts a selenium server before running the
tests. Another way to run the tests is using cabal directly by running `cabal test`.

## Deploying <a name="deploying"></a>

To deploy the site, use the provided `init-env.sh` script. Its usage is as follows:

```
Usage: init_env.sh [create|update] <stack-name> <cf-bucket> <cnames>
       init_env.sh init <cf-bucket>
       init_env.sh info <stack-name>
       init_env.sh [help|--help|-h]
```

First create required initial resources, then create the projects stack, thereby deploying the project.

```
./init-env.sh init <cf-bucket-name>
./init-env.sh create <stack-name> <cf-bucket-name> <cnames>
```

## Issues <a name="issues"></a>

Unfortunately, when trying to install the test-suite dependencies, `webdriver 0.6.1` fails to
install when using GHC 7.10 (*Jun 24, 2015*). This is due to a missing language pragma
`FlexibleContexts` in `src/Test/WebDriver/Commands.hs` (see
[bug ticket](https://github.com/kallisti-dev/hs-webdriver/issues/71). To get around this
problem you can download `webdriver` from hackage using `cabal get webdriver` and then fix the
problem yourself by adding the language pragma to the file `src/Test/WebDriver/Commands.hs`.
This issue has also been fixed upstream so you can use git to get the latest sources. Once the
working sources for `webdriver-0.6.1` are in place, run the following:

    $ cd webdriver-0.6.1
    $ cabal sandbox init
    $ cabal configure
    $ cabal install --only-dependencies
    $ cabal build
    $ cabal install
    $ cd ..
    $ cabal sandbox init --sandbox webdriver-0.6.1/.cabal-sandbox

This will share the cabal sandbox which we used to build and install our fixed version of
webdriver, with this project. Now the test-suite can be run once its dependencies are installed
(see [Running Tests](#running-tests)).

If you have an issue while browsing [my blog](http://blog.rekahsoft.ca) please file a bug using
my [flyspray powered bug tracker](https://bugs.rekahsoft.ca/index.php?project=9).

## Road Map <a name="road-map"></a>

There are still a few remaining rough edges to be fixed up. The ones I'm aware of are listed
below, if you find an issue please report it to me via email so I can make this site better.

* Include source files using some special syntax to avoid having to copy-paste source code into
articles. An example of what this could look like:

    \`\`\` {.haskell .lineNumber include="files/source/SomeFile.hs"}
    \`\`\`

* Have a proper draft system instead of copying articles to and from `drafts` and `posts`.
* Use LocalStorage API to save previously visited articles so they can be viewed offline (and
  saves on bandwidth). Note, this is already somewhat handled by the browsers cache, but the
  cache can be unreliable and also can't be controlled from javascript so a better solution is
  to use LocalStorage as mentioned.
* Make tag specific RSS feeds available via links (on individual tag pages and perhaps somewhere on the home page).

[Hakyll]: http://jaspervdj.be/hakyll/
[Clay]: http://fvisser.nl/clay/
[Skeleton]: http://www.getskeleton.com/
[JQuery]: http://jquery.com
[JQuery-address]: https://github.com/asual/jquery-address
[MathJax]: http://www.mathjax.org/
[Selenium]: http://docs.seleniumhq.org/
[Inkscape]: http://inkscape.org/
[Gimp]: http://www.gimp.org/
[Gnu Emacs]: http://www.gnu.org/software/emacs/
[Gnu Free Fonts]: http://www.gnu.org/software/freefont/

[GPLv3]: https://www.gnu.org/licenses/gpl.html
[Creative Commons BY-NC-SA License]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[Creative Commons BY-NC-ND License]: http://creativecommons.org/licenses/by-nc-nd/4.0/
