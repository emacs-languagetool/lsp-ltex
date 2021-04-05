[![Build Status](https://travis-ci.com/emacs-languagetool/lsp-ltex.svg?branch=master)](https://travis-ci.com/emacs-languagetool/lsp-ltex)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# lsp-ltex

`lsp-mode` client leveraging [LTEX Language Server](https://github.com/valentjn/ltex-ls).

<p align="center"><img src="./etc/screenshot.png"/></p>

## :floppy_disk: Quickstart

```el
(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp))))  ; or lsp-deferred
```

## :wrench: Configuration

`lsp-ltex` supports following configuration. Each configuration is described in
detail in [LTEX Settings](https://valentjn.github.io/vscode-ltex/docs/settings.html).

* `ltex.enabled` via `lsp-ltex-enabled`
* `ltex.language` via `lsp-ltex-language`
* `ltex.dictionary` via `lsp-ltex-dictionary`
* `ltex.disabledRules` via `lsp-ltex-disabled-rules`
* `ltex.enabledRules` via `lsp-ltex-enabled-rules`
* `ltex.hiddenFalsePositives` via `lsp-ltex-hidden-false-positives`
* `ltex.bibtex.fields` via `lsp-ltex-bibtex-fields`
* `ltex.latex.commands` via `lsp-ltex-latex-commands`
* `ltex.latex.environments` via `lsp-ltex-latex-environments`
* `ltex.markdown-nodes` via `lsp-ltex-markdown-nodes`
* `ltex.additionalRules.enablePickyRules` via `lsp-ltex-additional-rules-enable-picky-rules`
* `ltex.additionalRules.motherTongue` via `lsp-ltex-mother-tongue`
* `ltex.additionalRules.languageModel` via `lsp-ltex-additional-rules-language-model`
* `ltex.additionalRules.neuralNetworkModel` via `lsp-ltex-additional-rules-neural-network-model`
* `ltex.additionalRules.word2VecModel` via `lsp-ltex-additional-rules-word-2-vec-model`
* `ltex.ltex-ls.languageToolHttpServerUri` via `lsp-ltex-languagetool-http-server-uri`
* `ltex.ltex-ls.logLevel` via `lsp-ltex-log-level`
* `ltex.java.path` via `lsp-ltex-java-path`
* `ltex.java.forceTrySystemWide` via `lsp-ltex-java-force-try-system-wide`
* `ltex.java.initialHeapSize` via `lsp-ltex-java-initial-heap-size`
* `ltex.java.maximumHeapSize` via `lsp-ltex-java-maximum-heap-size`
* `ltex.sentenceCacheSize` via `lsp-ltex-sentence-cache-size`
* `ltex.diagnosticSeverity` via `lsp-ltex-diagnostic-severity`
* `ltex.checkFrequency` via `lsp-ltex-check-frequency`
* `ltex.clearDiagnosticsWhenClosingFile` via `lsp-ltex-clear-diagnostics-when-closing-file`
* `ltex.trace.server` via `lsp-ltex-trace-server`

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
