(when noninteractive
  (setq load-prefer-newer t)
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'ert)
(require 'divine-core)

(ert-deftest divine--numeric-argument-normalize ()
  (should (eq 12 (divine--numeric-argument-normalize 12)))
  (should (eq 1 (divine--numeric-argument-normalize 1)))
  (should (eq 1 (divine--numeric-argument-normalize nil)))
  (should (eq -1 (divine--numeric-argument-normalize '-)))
  (should (eq 33 (divine--numeric-argument-normalize '(33)))))

(ert-deftest divine--numeric-argument-p ()
  (should-not (divine-numeric-argument-p))
  (setq current-prefix-arg 4)
  (should (divine-numeric-argument-p))
  (should (eq 4 (divine-numeric-argument)))
  (should-not (divine-numeric-argument-p))
  (should (eq 1 (divine-numeric-argument))))

(ert-deftest divine--numeric-argument-flag ()
  (should-not (divine-numeric-argument-flag))
  (setq current-prefix-arg 4)
  (should (divine-numeric-argument-flag))
  (should-not (divine-numeric-argument-flag)))

(ert-deftest divine--reverse-direction-words ()
  (should (string=
           "previous previous backward left"
           (divine--reverse-direction-words "previous next forward right"))))

(ert-deftest divine-reverse-command ()
  (divine-reverse-command 'divine-numeric-argument divine-core-tests--numeric-arg-reversed)
  (setq current-prefix-arg 4)
  (should (eq -4 (divine-core-tests--numeric-arg-reversed))))

(when noninteractive
    (message "Divine %s\n" divine-version)
  (ert-run-tests-batch t))
