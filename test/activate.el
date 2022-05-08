;;; activate.el --- Test activation  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-05-08 12:50:37

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Test activation
;;

;;; Code:

(eask-pkg-init)

(require 'lsp-mode)

(lsp-install-server t 'ltex-ls)  ; Start installation

(defconst timeout 180
  "Timeout in seconds.")

(defvar timer 0)

(while (not (file-exists-p (lsp-ltex--extension-root)))
  (sit-for 5)
  (cl-incf timer 5)
  (message "Waited %s..." timer))

(unless (file-exists-p (lsp-ltex--extension-root))
  (error "Failed to install server: %s" (lsp-ltex--extension-root))
  (kill-emacs 1))

(message "Testing with a file...")

(find-file "README.md")  ; start lsp

;;; activate.el ends here
