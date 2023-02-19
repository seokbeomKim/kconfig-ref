;;; kconfig-ref.el --- Minor mode to parse kconfig
;;
;; Copyright (C) 2023 Jason Kim
;;
;; Author: Jason Kim <sukbeom.kim@gmail.com>
;; Maintainer: Jason Kim <sukbeom.kim@gmail.com>
;; Created: February 19, 2023
;; Modified: February 19, 2023
;; Version: 0.1.0
;; Keywords: kconfig, linux, kernel
;; Homepage: https://github.com/sukbeom/package
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a minor mode to look up kconfig with parsing .config file in linux
;; kernel directory.

;; Setup:

;; (require 'kconfig-ref)

;;; Code:

(require 'ripgrep)

(defvar kconfig-ref-config-file ".config")
(setq kconfig-ref-last-find-config nil)

(defun kconfig-ref-config-file-exist ()
  (file-exists-p (concat (projectile-acquire-root) ".config")))

(defun kconfig-ref-parse-item (line)
  "Add dependency information through parsing .config"
  (when (string-match "depends on" line)
    ;; if .config does not exist, skip to parse .config file
    (if (not (kconfig-ref-config-file-exist))
        (message "No .config in the kernel root directory.")
      (progn
        (let ((dependencies (substring line (match-end 0))))
          (dolist (depend (split-string dependencies "&&\\|||"))
            (message (string-trim depend))
            (let ((depend-on (string-trim depend)))
              (setq backup-buffer (current-buffer))
              (find-file (concat (projectile-acquire-root) kconfig-ref-config-file))
              (goto-char (point-min))
              (condition-case err
                  (progn
                    (when (re-search-forward (concat "CONFIG_" depend-on "=\\(.*\\)"))
                      (setq line (replace-regexp-in-string depend-on
                                                           (concat depend-on "[=" (match-string 1) "]")
                                                           line))))
                (search-failed (message "No match found in .config")))
              (switch-to-buffer backup-buffer)))))))
  line)

(defun kconfig-ref-find-file-hook ()
  "Ripgrep search hook"

  (let ((output-buffer-name "*kconfig-ref*")
        (kconfig-output-string "")
        (ripgrep-search-buffer-name (concat "*ripgrep-search*<" (projectile-project-name) ">")))

    (get-buffer-create output-buffer-name)
    (with-current-buffer ripgrep-search-buffer-name
      (message (buffer-name))
      (goto-char (point-min))
      (when (re-search-forward "\\(.*\\):\\(.*\\):\\([ \t]*config.*\\)")
        (let ((kconfig-path (match-string 1))
              (kconfig-lnum (match-string 2))
              (kconfig-item (match-string 3))
              (kconfig-done nil))
          (setq kconfig-output-string
                (concat (format "Found in [[%s::%s][%s]]\n\n"
                                (concat (projectile-acquire-root) kconfig-path)
                                kconfig-lnum
                                kconfig-path)
                        kconfig-ref-last-find-config "\n"))
          (find-file kconfig-path)
          (goto-char (point-min))
          (search-forward kconfig-item)
          (forward-line 1)
          (beginning-of-line)
          (while (and
                  (not (eobp))
                  (equal kconfig-done nil))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              ;; Check block is ended
              (if (not (string-match "^[ \t]*config [A-Z_]+" line))
                  (progn
                    (setq kconfig-output-string
                          (concat kconfig-output-string (kconfig-ref-parse-item line) "\n"))
                    ;; (setq kconfig-output-string (concat kconfig-output-string line "\n"))
                    (forward-line 1))
                (setq kconfig-done t))))))
      (kill-buffer ripgrep-search-buffer-name))
    (with-current-buffer output-buffer-name
      (erase-buffer)
      (insert kconfig-output-string "\n")
      (other-window 1)
      (switch-to-buffer output-buffer-name)
      (org-mode)
      (read-only-mode t)
      (goto-char (point-min))))
  (other-window 1)
  (switch-to-buffer kconfig-ref-backup-buffer))

(defun kconfig-ref-find-file-with-name (name)
  (let ((config-key (concat "config " name)))
    (setq kconfig-ref-last-find-config config-key)
    (add-hook 'ripgrep-search-finished-hook 'kconfig-ref-find-file-hook)
    (ripgrep-regexp config-key
                    (projectile-acquire-root))))

(defun kconfig-ref-find-config ()
  "Find kconfig deinifition under the cursor"
  (interactive)
  (setq kconfig-ref-backup-buffer (current-buffer))
  (setq-local valid-input (replace-regexp-in-string "^CONFIG_" "" (current-word)))
  (kconfig-ref-find-file-with-name valid-input))

(provide 'kconfig-ref)
;;; package.el ends here
