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
 ((guix licenses) #:prefix license:)
 (guix packages)
 (guix build-system haskell)
 (rekahsoft-gnu packages haskell-web))

(define-public blog-rekahsoft-ca
  (package
    (name "blog-rekahsoft-ca")
    (version "0.0.0-0")
    (source #f)
    (build-system haskell-build-system)
    (inputs `(("ghc-hakyll" ,ghc-hakyll)
              ("ghc-clay" ,ghc-clay)))
    (home-page "http://git.rekahsoft.ca/rekahsoft/blog-rekahsoft-ca")
    (synopsis "Code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca")
    (description
     "The code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca.")
    (license license:gpl3)))

blog-rekahsoft-ca
