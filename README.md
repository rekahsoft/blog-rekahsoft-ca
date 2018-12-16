# Source Code for **[#! Lambda Slang](http://www.blog.rekahsoft.ca)**

* [Features](#features)
* [Tools](#tools)
* [License](#license)
* [Building](#building)
* [Deploying](#deploying)
* [Issues](#issues)

[#! Lambda Slang](http://www.blog.rekahsoft.ca) is the personal technical blog of *Collin Doering*,
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

[Stack][] is used to manage dependencies for this project. A simple wrapper script `site` is
provided that also takes care of building the static site and offering access to hakyll
commands.

    $ ./site build
    $ ./site watch

## Deploying <a name="deploying"></a>

Terraform is used to deploy this site. Its configuration files are located in `./infra`. Three
workspaces are currently available, including:

  - default (unused)
  - staging
  - production

For example, this is how to deploy the production version of the site:

    $ cd infra
    $ terraform workspace select production
    $ terraform plan --var-file=production.tfvars --out local.plan
    $ terraform apply local.plan

## Issues <a name="issues"></a>

If you have an issue while browsing [my blog](http://www.blog.rekahsoft.ca) please file a issue
in the [blog-rekahsoft-ca](https://git.rekahsoft.ca/rekahsoft/blog-rekahsoft-ca/issues) issue
tracker.

[Hakyll]: http://jaspervdj.be/hakyll/
[Clay]: http://fvisser.nl/clay/
[Skeleton]: http://www.getskeleton.com/
[JQuery]: http://jquery.com
[JQuery-address]: https://github.com/asual/jquery-address
[MathJax]: http://www.mathjax.org/
[Inkscape]: http://inkscape.org/
[Gimp]: http://www.gimp.org/
[Stack]: https://haskellstack.org
[Gnu Emacs]: http://www.gnu.org/software/emacs/
[Gnu Free Fonts]: http://www.gnu.org/software/freefont/

[GPLv3]: https://www.gnu.org/licenses/gpl.html
[Creative Commons BY-NC-SA License]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[Creative Commons BY-NC-ND License]: http://creativecommons.org/licenses/by-nc-nd/4.0/
