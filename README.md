# kconfig-ref

kconfig-ref is an Emacs package that helps you find the Kconfig implementation
of a symbol under the cursor quickly.

![kconfig-ref-usage](https://github.com/seokbeomKim/kconfig-ref/blob/images/kconfig-ref-screenshot.gif "kconfig-ref usage")

## Installation

```
(require 'kconfig-ref)
```

## Motivation

The motivation behind kconfig-ref is to make it easier and quicker to find
Kconfig symbols and their dependencies in the .config file.


## Usage

The main command is :

` M-x kconfig-ref-init-db ` which initializes the kconfig database. This takes
some time at first. (This will be optimized in the future with further work.)

` M-x kconfig-ref-parse-dotconfig ` which parses .config file in the Linux
kernel.

` M-x kconfig-ref-find-config-by-name ` which finds the definition of the
Kconfig symbol by the name. You can use `*` symbol for likewise search.

` M-x kconfig-ref-find-config-at-cursor ` which finds the definition of the
Kconfig symbol under the cursor.

` M-x kconfig-ref-parse-current-buffer ` which updates the current buffer
(Kconfig)'s change. This should work with kconfig-mode-hook.

## Feedback

If you have any feedback, suggestions for improvements or advice, please feel
free to get in touch. 
