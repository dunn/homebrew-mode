# hombrew-mode

Emacs minor mode for editing [Homebrew](http://brew.sh) formulae.

## setup

```elisp
(add-to-list 'load-path "/where/is/homebrew-mode")
(require 'homebrew-mode)
(homebrew-mode-default-hooks)
```

## keys and commands

The command prefix is <kbd>C-c h</kbd>.  These are the commands currently mapped to it:

- <kbd>C-c h f</kbd>: Download the source file(s) for the formula
  in the current buffer.

- <kbd>C-c h u</kbd>: Download and unpack the source file(s) for the formula
  in the current buffer.

- <kbd>C-c h i</kbd>: Install the formula in the current buffer.

- <kbd>C-c h r</kbd>: Uninstall the formula in the current buffer.

- <kbd>C-c h t</kbd>: Run the test for the formula in the current buffer.

- <kbd>C-c h a</kbd>: Audit the formula in the current buffer.

- <kbd>C-c h c</kbd>: Open a dired buffer in the Homebrew cache
  (default `/Library/Caches/Homebrew`).

- <kbd>C-c h p</kbd>: Insert Python `resource` blocks (requires poet,
  installed with `pip install homebrew-pypi-poet`).

## custom variables

- If you’re using Linuxbrew or a non-standard prefix on Mac OS, you’ll
  need to update `homebrew-prefix` to point at your `brew –-prefix`.

- If you’re using Linuxbrew or have your cache in a non-standard
  location on Mac OS, update `homebrew-cache-dir`.

- If you want to turn on whitespace-mode when editing formulae that
  have inline patches, set `homebrew-patch-whitespace-mode` to
  `t`. It’s off by default since it looks ugly.
