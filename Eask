;; -*- mode: eask; lexical-binding: t -*-

(package "lsp-ltex"
         "0.2.1"
         "LSP Clients for LTEX")

(website-url "https://github.com/emacs-languagetool/lsp-ltex")
(keywords "convenience" "lsp" "languagetool" "checker")

(package-file "lsp-ltex.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")
(source "jcs-elpa")

(depends-on "emacs" "28.1")
(depends-on "lsp-mode")
(depends-on "github-tags")  ; TODO: Move to `development' scope?

(development
 (depends-on "markdown-mode"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t
                   byte-compile-docstring-max-column 1000)))
