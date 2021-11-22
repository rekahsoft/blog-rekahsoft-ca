;; (C) Copyright Collin J. Doering 2021
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; File: blog-rekahsoft-ca.scm
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Nov 21, 2021

(use-modules
 (gnu packages)
 ((guix licenses) #:prefix license:)
 (guix packages)
 (guix download)
 (guix git-download)
 (guix build-system haskell)
 (gnu packages linux)
 (gnu packages haskell-web)
 (gnu packages haskell-apps)
 (gnu packages haskell-xyz)
 (gnu packages haskell-crypto)
 (gnu packages haskell-check))

(define-public ghc-lrucache
  (package
    (name "ghc-lrucache")
    (version "1.2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/lrucache/lrucache-"
               version
               ".tar.gz"))
        (sha256
          (base32 "11avhnjnb89rvn2s41jhh5r40zgp7r6kb5c0hcfiibpabqvv46pw"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-contravariant" ,ghc-contravariant)))
    (home-page "http://github.com/chowells79/lrucache")
    (synopsis "a simple, pure LRU cache")
    (description
      "This package contains a simple, pure LRU cache, implemented in terms of \"Data.Map\". . It also contains a mutable IO wrapper providing atomic updates to an LRU cache.")
    (license license:bsd-3)))

(define-public ghc-hakyll
  (package
    (name "ghc-hakyll")
    (version "4.15.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/hakyll/hakyll-"
               version
               ".tar.gz"))
        (sha256
          (base32 "18nhpnhi63xvmb14khw1ad8rwj2lxdyhpc58gnmr9vb5zz2db6bh"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-aeson" ,ghc-aeson)
        ("ghc-blaze-html" ,ghc-blaze-html)
        ("ghc-blaze-markup" ,ghc-blaze-markup)
        ("ghc-data-default" ,ghc-data-default)
        ("ghc-file-embed" ,ghc-file-embed)
        ("ghc-hashable" ,ghc-hashable)
        ("ghc-lifted-async" ,ghc-lifted-async)
        ("ghc-lrucache" ,ghc-lrucache)
        ("ghc-network-uri" ,ghc-network-uri)
        ("ghc-optparse-applicative" ,ghc-optparse-applicative)
        ("ghc-random" ,ghc-random)
        ("ghc-regex-tdfa" ,ghc-regex-tdfa)
        ("ghc-resourcet" ,ghc-resourcet)
        ("ghc-scientific" ,ghc-scientific)
        ("ghc-tagsoup" ,ghc-tagsoup)
        ("ghc-time-locale-compat" ,ghc-time-locale-compat)
        ("ghc-unordered-containers" ,ghc-unordered-containers)
        ("ghc-vector" ,ghc-vector)
        ("ghc-yaml" ,ghc-yaml)
        ("ghc-wai" ,ghc-wai)
        ("ghc-warp" ,ghc-warp)
        ("ghc-wai-app-static" ,ghc-wai-app-static)
        ("ghc-http-types" ,ghc-http-types)
        ("ghc-fsnotify" ,ghc-fsnotify)
        ("ghc-http-conduit" ,ghc-http-conduit)
        ("ghc-pandoc" ,ghc-pandoc)))
    (native-inputs
      `(("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-golden" ,ghc-tasty-golden)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)
        ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
        ;; "rev" program used during tests from util-linux
        ("util-linux" ,util-linux)))
    (arguments
      `(#:cabal-revision
        ("1" "1kxdvh7250nvfdz5wnchyqhghhp05i06sfnjg9ar17p0wyqzv46z")))
    (home-page "http://jaspervdj.be/hakyll")
    (synopsis "A static website compiler library")
    (description
      "Hakyll is a static website compiler library. It provides you with the tools to create a simple or advanced static website using a Haskell DSL and formats such as markdown or RST. You can find more information, including a tutorial, on the website: . * <http://jaspervdj.be/hakyll> . If you seek assistance, there's: . * A google group: <http://groups.google.com/group/hakyll> . * An IRC channel, @#hakyll@ on irc.libera.chat (we *do not* have a channel on Freenode anymore) . Additionally, there's the Haddock documentation in the different modules, meant as a reference.")
    (license license:bsd-3)))



(define-public ghc-hspec-discover
  (package
    (name "ghc-hspec-discover")
    (version "2.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/hspec-discover/hspec-discover-"
               version
               ".tar.gz"))
        (sha256
          (base32 "13cbjyzmd543jcpi7bh420adh2bpn088v8fv0cb25zgx8q5khmxw"))))
    (build-system haskell-build-system)
    (native-inputs
      `(("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-hspec-meta" ,ghc-hspec-meta)
        ("ghc-mockery" ,ghc-mockery)))
    (home-page "http://hspec.github.io/")
    (synopsis "Automatically discover and run Hspec tests")
    (description
      "Automatically discover and run Hspec tests . <http://hspec.github.io/hspec-discover.html>")
    (license license:expat)))

(define-public ghc-clay
  (package
    (name "ghc-clay")
    (version "0.13.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/clay/clay-"
               version
               ".tar.gz"))
        (sha256
          (base32 "192lsbyj6azjs2ygpx4i47fyr8zfmvwcas8mia07ndqglk2c9csx"))))
    (build-system haskell-build-system)
    (native-inputs
      `(("ghc-hspec" ,ghc-hspec) ("ghc-hspec-discover" ,ghc-hspec-discover)))
    (home-page "http://fvisser.nl/clay")
    (synopsis "CSS preprocessor as embedded Haskell.")
    (description
      "Clay is a CSS preprocessor like LESS and Sass, but implemented as an embedded domain specific language (EDSL) in Haskell. This means that all CSS selectors and style rules are first class Haskell functions, which makes reuse and composability easy. . The project is described on <http://fvisser.nl/clay>. . The API documentation can be found in the top level module \"Clay\".")
    (license license:bsd-3)))


(define-public blog-rekahsoft-ca
    (package
      (name "blog-rekahsoft-ca")
      (version "0.0.0-0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.rekahsoft.ca/rekahsoft/blog-rekahsoft-ca.git")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kjb6gf7fbydw8g065kq350swgpmk02lk64dvhn3fwwgqrdgy6hh"))))
      (build-system haskell-build-system)
      (inputs `(("ghc-hakyll" ,ghc-hakyll)
                ("ghc-clay" ,ghc-clay)))
      (home-page "http://git.rekahsoft.ca/rekahsoft/blog-rekahsoft-ca")
      (synopsis "Code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca")
      (description
       "The code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca.")
      (license license:gpl3)))

;; (define-public blog-rekahsoft-ca-site
;;     (package
;;       (inherit blog-rekahsoft-ca)
;;       (name "blog-rekahsoft-ca-site")
;;       (build-system haskell-build-system)
;;       (native-inputs `(("cabal-install" ,cabal-install)))
;;       (inputs `(("ghc-hakyll" ,ghc-hakyll)
;;                 ("ghc-clay" ,ghc-clay)))
;;       (home-page "http://git.rekahsoft.ca/rekahsoft/blog-rekahsoft-ca")
;;       (synopsis "Code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca")
;;       (description
;;        "The code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca.")
;;       (license license:gpl3)))


blog-rekahsoft-ca
