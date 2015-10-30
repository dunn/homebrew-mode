(require 'ert)
(require 'homebrew-mode)

(ert-deftest formula-detection ()
  (should (equal t (homebrew--formula-file-p "/usr/local/Library/Formula/when.rb")))
  (should (equal t (homebrew--formula-file-p "/usr/local/Library/Taps/homebrew/homebrew-emacs/Formula/olivetti.rb")))
  (should (equal t (homebrew--formula-file-p "/usr/local/Library/Taps/homebrew/homebrew-versions/play12.rb")))
  (should (equal t (homebrew--formula-file-p "/usr/local/Library/Taps/homebrew/homebrew-test/HomebrewFormula/honk.rb")))

  ;; alternative install prefix
  (should (equal t (homebrew--formula-file-p "/home/user/.linuxbrew/Library/Formula/when.rb")))
  (should (equal t (homebrew--formula-file-p "/home/user/.linuxbrew/Library/Taps/homebrew/homebrew-emacs/Formula/olivetti.rb")))
  (should (equal t (homebrew--formula-file-p "/home/user/.linuxbrew/Library/Taps/homebrew/homebrew-x11/xpdf.rb")))
  (should (equal t (homebrew--formula-file-p "/home/user/.linuxbrew/Library/Taps/homebrew/homebrew-test/HomebrewFormula/honk.rb")))

  ;; non-formula files
  (should (equal nil (homebrew--formula-file-p "/usr/local/Library/Homebrew/utils.rb")))
  (should (equal nil (homebrew--formula-file-p "/usr/local/Library/Taps/homebrew/homebrew-emacs/cmd/emacs.rb")))
)
