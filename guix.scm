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
 (guix git-download)
 (guix gexp)
 (gnu packages base)
 (gnu packages javascript)
 (gnu packages haskell-apps)
 (rekahsoft-gnu packages haskell-web)
 (git))

(define %srcdir
  (dirname (current-filename)))

(define %blog-rekahsoft-ca
  (let ((commit (oid->string
                 (reference-target
                  (repository-head (repository-open %srcdir)))))
        (revision "1"))
    (package
      (name "blog-rekahsoft-ca")
      (version (git-version "0.0.0.0" revision commit))
      (source (local-file "." "blog-rekahsoft-ca-git-checkout"
                          #:recursive? #t
                          #:select? (git-predicate %srcdir)))
      (build-system haskell-build-system)
      (native-inputs `(("glibc-utf8-locales" ,glibc-utf8-locales)
                       ("make" ,gnu-make)
                       ("ghcid" ,ghcid)))
      (inputs `(("ghc-hakyll" ,ghc-hakyll)
                ("ghc-clay" ,ghc-clay)
                ("js-mathjax" ,js-mathjax)))
      (outputs '("out" "site" "static"))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-site-script
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (setenv "PATH" (string-append out "/bin:" (getenv "PATH")))
                 (symlink (string-append out "/bin/blog-rekahsoft-ca") (string-append out "/bin/site"))
                 #t)))
           (add-after 'install-site-script 'build-site
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (site (assoc-ref outputs "site")))
                 ;; Copy (vendor) dependencies: MathJax
                 (copy-recursively (string-append (assoc-ref %build-inputs "js-mathjax")
                                                  "/share/javascript/mathjax")
                                   "lib/MathJax" #:follow-symlinks? #t)

                 ;; All source files are read-only and need to be adjusted to allow the
                 ;; site to be generated at the end of the build
                 (for-each make-file-writable (find-files "."))

                 (invoke "site" "build")
                 (copy-recursively "_site" site)
                 #t))))))
      (home-page "http://git.rekahsoft.ca/rekahsoft/blog-rekahsoft-ca")
      (synopsis "Code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca")
      (description
       "The code, templates and content for my Hakyll powered blog at blog.rekahsoft.ca.")
      (license license:gpl3))))

%blog-rekahsoft-ca
