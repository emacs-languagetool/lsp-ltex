;;; lsp-ltex.el --- LSP Clients for LTEX  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-04-03 00:35:56

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: LSP Clients for LTEX.
;; Keyword: lsp languagetool checker
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (lsp-mode "6.1"))
;; URL: https://github.com/emacs-languagetool/lsp-ltex

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
;; LSP server implementation for LTEX
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ltex nil
  "Settings for the LTEX Language Server.

https://github.com/valentjn/ltex-ls"
  :prefix "lsp-ltex-"
  :group 'lsp-mode
  :link '(url-link :tag "Github" "https://github.com/emacs-languagetool/lsp-ltex"))

(provide 'lsp-ltex)
;;; lsp-ltex.el ends here
