;;; language-modes

(use-package "python-mode"
  :ensure t
  :mode ("\.py$"))


(use-package "go-mode"
  :ensure t
  :mode ("\.go$"))


(use-package "rust-mode"
  :ensure t
  :mode ("\.rs$"))


(use-package "zig-mode"
  :ensure t
  :mode ("\.zig$"))


(require 'flex)       ; (f)lex scanner generator language mode
(require 'bison-mode) ; bison / yacc parser generator language mode

