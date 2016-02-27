;;; homebrew-mode-ert.el --- ERT tests for homebrew-mode

;; Copyright (C) 2015, 2016 Alex Dunn

;; This file is part of homebrew-mode

;; homebrew-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
