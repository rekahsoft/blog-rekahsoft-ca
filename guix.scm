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
 (gnu packages base)
 (rekahsoft-gnu packages haskell-web))

(define release-version "0.0.0.0")

(define-public blog-rekahsoft-ca
  (package
    (name "blog-rekahsoft-ca")
    (version release-version)
    (source (string-append "./dist/blog-rekahsoft-ca-" release-version ".tar.gz"))
    (build-system haskell-build-system)
    (native-inputs `(("glibc-utf8-locales" ,glibc-utf8-locales)))
    (inputs `(("ghc-hakyll" ,ghc-hakyll)
              ("ghc-clay" ,ghc-clay)))
    (outputs `("out" "site" "static"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-site-script
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PATH" (string-append out "/bin:" (getenv "PATH")))
               (install-file "site" (string-append out "/bin/"))
               #t)))
         (add-after 'install-site-script 'build-site
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site (assoc-ref outputs "site")))
               (setenv "LANG" "en_US.UTF-8")

               ;; For some reason, all files are read-only and need to be adjusted to allow the
               ;; site to be generated
               (for-each make-file-writable (find-files "."))

               (invoke "site" "build")
               (copy-recursively "_site" site)
               #t))))))
    (home-page "http://git.rekahsoft.ca/rekahsoft/blog-rekahsoft-ca")
    (synopsis "Code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca")
    (description
     "The code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca.")
    (license license:gpl3)))

blog-rekahsoft-ca
