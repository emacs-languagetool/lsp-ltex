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

(defcustom lsp-ltex-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with LTEX Language Server."
  :type 'list
  :group 'lsp-ltex)

(defcustom lsp-ltex-version "10.0.0"
  "Version of LTEX language server."
  :type 'string
  :group 'lsp-ltex)

(defcustom lsp-ltex-extension-name
  (format "ltex-ls-%s.tar.gz" lsp-ltex-version)
  "File name of the extension file from language server."
  :type 'string
  :group 'lsp-ltex)

(defcustom lsp-ltex-server-download-url
  (format "https://github.com/valentjn/ltex-ls/releases/download/%s/%s"
          lsp-ltex-version lsp-ltex-extension-name)
  "Automatic download url for lsp-ltex."
  :type 'string
  :group 'lsp-ltex)

(defcustom lsp-ltex-server-store-path
  (f-join lsp-server-install-dir "ltex-ls")
  "The path to the file in which LTEX Language Server will be stored."
  :type 'file
  :group 'lsp-ltex)

(defcustom lsp-ltex-language "en-US"
  "The language LanguageTool should check against"
  :type 'string
  :group 'lsp-ltex)

(defun lsp-ltex--execute (cmd &rest args)
  "Return non-nil if CMD executed succesfully with ARGS."
  (save-window-excursion
    (let ((inhibit-message t) (message-log-max nil))
      (= 0 (shell-command (concat cmd " "
                                  (mapconcat #'shell-quote-argument args " ")))))))

(defun lsp-ltex--downloaded-extension-path ()
  "Return full path of the downloaded extension.

This is use to unzip the language server files."
  (f-join lsp-ltex-server-store-path lsp-ltex-extension-name))

(defun lsp-ltex--extension-root ()
  "Return the root of the extension path.

This is use to active language server and check if language server's existence."
  (f-join lsp-ltex-server-store-path (format "ltex-ls-%s" lsp-ltex-version)))

(defun lsp-ltex--server-entry ()
  "Return the server entry file.

This file is use to activate the language server."
  (f-join (lsp-ltex--extension-root) "bin" (if (eq system-type 'windows-nt)
                                               "ltex-ls.bat"
                                             "ltex-ls")))

(defun lsp-ltex--server-command ()
  "Startup command for LTEX language server."
  (list (lsp-ltex--server-entry)))

(lsp-register-custom-settings
 '(("ltex.language" lsp-ltex-language)
   ))

(lsp-dependency
 'ltex-ls
 '(:system "ltex-ls")
 `(:download :url lsp-ltex-server-download-url
             :store-path ,(lsp-ltex--downloaded-extension-path)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-ltex--server-command
                   (lambda () (f-exists? (lsp-ltex--extension-root))))
  :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
  :priority 7
  :server-id 'ltex-ls
  :download-server-fn
  (lambda (_client _callback error-callback _update?)
    (lsp-package-ensure
     'ltex-ls
     (lambda ()
       (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
         ;; TODO: Error handling when unzip failed
         (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path)
                            "-C" dest)))
     error-callback))))

(provide 'lsp-ltex)
;;; lsp-ltex.el ends here
