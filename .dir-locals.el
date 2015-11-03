((emacs-lisp-mode
  (compile-command . "cask exec ert-runner")
  (eval ignore-errors
        (push (quote ("Tests" "(\\(\\<ert-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?" 2)) imenu-generic-expression))))
