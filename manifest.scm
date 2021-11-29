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

;; File: manifest.scm
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Nov 21, 2021

(use-modules
 (gnu packages)
 (guix packages)
 (guix profiles)
 (guix transformations))

(load "guix.scm")

(setenv "PS1" "\\W [env]\\$ ")

(define dev-transform
  (options->transformation
   `((with-source . ,(string-append "blog-rekahsoft-ca=" (getcwd))))))

(concatenate-manifests
 (list
  (packages->manifest
   `(,(dev-transform blog-rekahsoft-ca)))
  (specifications->manifest
   `("coreutils"))))
