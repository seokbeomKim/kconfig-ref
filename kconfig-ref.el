;;; kconfig-ref.el --- A simple package for looking up kconfig symbol quickly
;;
;; Copyright (C) 2023 Jason Kim
;;
;; Author: Jason Kim <sukbeom.kim@gmail.com>
;; Maintainer: Jason Kim <sukbeom.kim@gmail.com>
;; Created: February 19, 2023
;; Modified: July 23, 2023
;; Version: 0.2.1
;; Keywords: tools, kconfig, linux, kernel
;; Homepage: https://github.com/seokbeomkim/kconfig-ref
;; Package-Requires: ((emacs "24.4") (projectile "2.7.0") emacsql)

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

;; This is a simple package for looking up kconfig symbols and values with
;; parsing the .config file in the Linux kernel directory.

;; Setup:

;; (require 'kconfig-ref)
;;; Code:
(require 'projectile)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'json)

(defvar kconfig-ref--progress-reporter nil)
(defvar kconfig-ref--progress-count 0)
(defvar kconfig-ref--progress-max 0)
(defvar kconfig-ref--flag-update-buffer nil)
(defvar kconfig-ref--default-arch "arm64")

;;;###autoload
(defun kconfig-ref-config-file-exist ()
  "Check .config exists in the Linux kernel source tree."
  (let ((dotfile-path (expand-file-name ".config" (projectile-acquire-root))))
    (if (file-exists-p dotfile-path)
        dotfile-path
      nil)))

;;;###autoload
(defun kconfig-ref-find-config-at-cursor ()
  "Find kconfig deinifition under the cursor."
  (interactive)
  (kconfig-ref--set-db-filepath)
  (setq-local valid-input (replace-regexp-in-string "^CONFIG_" "" (current-word)))
  (kconfig-ref-find-config-by-name valid-input))

(defun kconfig-ref-get-projectile-root-dir ()
  "Get projectile root directory path.
If the return value is nil then return error."
  (or (projectile-project-root)
      (error "Invalid projectile root directory")))

(defvar kconfig-ref-db-filename "kconfig-ref.db")
(defvar kconfig-ref-db-filepath (format "~/%s" kconfig-ref-db-filename))
(defvar kconfig-ref-db nil)

(defun kconfig-ref--init-table (&optional force)
  "Initialize a config table.
If FORCE is true, re-create the table after remove the previous one."
  (if force
      (ignore-errors
        (emacsql kconfig-ref-db [:drop-table configs])))
  (emacsql kconfig-ref-db
           [:create-table configs
                          ([(id integer :primary-key :autoincrement) ; primary key
                            name                                     ; config name
                            (type integer)                           ; 0: menuconfig, 1: config
                            file_loc                                 ; file location
                            line_num                                 ; line number
                            conf_val                                 ; config value
                            dirty])]))                               ; dirty bit

(defun kconfig-ref--get-random-uuid ()
  "Insert a random UUID."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))

(defun kconfig-ref--set-db-filepath ()
  "Set kconfig-ref database filepath."
  (let ((session-private-dir (kconfig-ref--init-private-directory))
        (db-json-file-name "db.json")
        (db-json-file-path nil)
        (parsed-data nil)
        (json-data nil)
        (new-uuid nil)
        (project-root-dir (kconfig-ref-get-projectile-root-dir)))
    (setq db-json-file-path (expand-file-name db-json-file-name session-private-dir))
    (if (file-exists-p db-json-file-path)
        (with-temp-buffer (insert-file-contents db-json-file-path)
                          (setq parsed-data (json-read-from-string (buffer-string)))))
    (if (catch 'session-db
          (dolist (p parsed-data)
            (when (string= (car p) project-root-dir)
              (setq kconfig-ref-db-filepath (expand-file-name (cdr p) session-private-dir))
              (throw 'session-db nil)))
          t)
        (progn
          (setq new-uuid (kconfig-ref--get-random-uuid))
          (setq kconfig-ref-db-filepath (expand-file-name new-uuid session-private-dir))
          (push (cons project-root-dir new-uuid) parsed-data)))
    (setq json-data (json-encode parsed-data))
    (write-region json-data nil db-json-file-path))
  (message kconfig-ref-db-filepath)
  (setq kconfig-ref-db (emacsql-sqlite kconfig-ref-db-filepath)))

(defun kconfig-ref--create-db (projectile-rootdir &optional force)
  "Create a DB in projectile root directory.
Initialize `configs' table as well. `PROJECTILE-ROOTDIR' is essential.
Optional argument FORCE Create the database by force. This option will remove out the previous one. ."
  (if projectile-rootdir
      (progn
        (kconfig-ref--set-db-filepath)
        (condition-case err
            (kconfig-ref--init-table force)
          (error "Failed to create db: " err)))
    (error (format "Invalid projectile-rootdir %s" projectile-rootdir))))

(defun kconfig-ref--get-syntax-type (syntax)
  "Get Kconfig SYNTAX from the parsed line."
  (let ((kconfig-syntax-type1 '("source" "menuconfig" "config"))
        (kconfig-syntax-type2 '("depends" "select"))
        res)
    (dolist (keyword kconfig-syntax-type1)
      (if (string= keyword (string-trim-right syntax))
          (setq res keyword)))
    (dolist (keyword kconfig-syntax-type2)
      (if (string= keyword (string-trim-left syntax))
          (setq res keyword)))
    res))

(defun kconfig-ref--add-item (config type filepath line-number)
  "Insert a new item.
Argument CONFIG the name of the config-item.
Argument TYPE 0: menuconfig, 1: config.
Argument FILEPATH Kconfig file path.
Argument LINE-NUMBER Line number of the config."
  (emacsql kconfig-ref-db
           [:insert :into configs
            :values $v1]
           (list (vector nil config type filepath line-number "is not set" 0))))

(defun kconfig-ref--update-item (config type filepath line-number)
  "Update the record.
`dirty' is a flag used to clean up unused CONFIGs but defined
previously.
Argument CONFIG the config name.
Argument TYPE 0: menuconfig, 1: config.
Argument FILEPATH Kconfig file path.
Argument LINE-NUMBER Line number of the config."
  (let ((target-id (car (car (emacsql kconfig-ref-db
                                      [:select id :from configs
                                       :where (and (= name $s1)
                                                   (= file-loc $s2))]
                                      config filepath)))))
    (if target-id
        (emacsql kconfig-ref-db
                 [:update configs
                  :set [(= file-loc $s1) (= line_num $s2) (= dirty 0)]
                  :where (= id $s3)]
                 filepath line-number target-id)
      (kconfig-ref--add-item config type filepath line-number))))

(defun kconfig-ref--parse-menuconfig (config filepath line-number)
  "Parse a menuconfig and insert it into the database.
Argument CONFIG
Argument FILEPATH Kconfig file path.
Argument LINE-NUMBER the line number of the config."
  (if kconfig-ref--flag-update-buffer
      (kconfig-ref--update-item config 0 filepath line-number)
    (kconfig-ref--add-item config 0 filepath line-number)))

(defun kconfig-ref--parse-config (config filepath line-number)
  "Parse a CONFIG and insert it into the database.
Argument FILEPATH Kconfig filepath.
Argument LINE-NUMBER Line number of the config item."
  (if kconfig-ref--flag-update-buffer
      (kconfig-ref--update-item config 1 filepath line-number)
    (kconfig-ref--add-item config 1 filepath line-number)))

(defun kconfig-ref--find-last-record-id ()
  "Return the primary key of the last record."
  (car (car (emacsql kconfig-ref-db
                     [:select (funcall max id) :from configs]))))

(defun kconfig-ref--find-config-by-name (name)
  "Get config description by the NAME."
  (kconfig-ref--set-db-filepath)
  (let ((trimmed-name (string-trim-left name "CONFIG_")))
    (setq trimmed-name (replace-regexp-in-string "*" "%%" name))
    (if (= 0 (length trimmed-name))
        (setq trimmed-name "%%"))
    (emacsql kconfig-ref-db
             [:select * :from configs :where (like name $s1)]
             (format "%s" trimmed-name))))

(defvar kconfig-ref--choice-list nil)
(defun kconfig-ref-find-config-by-name-choose (choice)
  "Get the selection when the user try to find the config.
Argument CHOICE user's selection."
  (interactive
   (list (completing-read "Choose: "
                          kconfig-ref--choice-list nil t)))
  (car (split-string choice " ")))

(defun kconfig-ref--update-dot-value (conf val)
  "Update dot config value.
Argument CONF config name.
Argument VAL config value."
  (emacsql kconfig-ref-db
           [:update configs
            :set [(= conf_val $s2)]
            :where (= name $s1)]
           (string-trim-left conf "CONFIG_") val))

(defun kconfig-ref--apply-dot-config (conf)
  "Parse config and update the database.
Argument CONF config name."
  (let ((splitted (split-string conf "=")))
    (if (null (string-match "\\# CONFIG_" conf))
        (kconfig-ref--update-dot-value (nth 0 splitted) (nth 1 splitted))
      (kconfig-ref--update-dot-value
       (car (split-string conf " " t "#"))
       "is not set"))))

(defun kconfig-ref-parse-dotconfig ()
  "Update dot config to database."
  (interactive)
  (kconfig-ref--set-db-filepath)
  (condition-case err
      (with-temp-buffer
        (insert-file-contents (kconfig-ref-config-file-exist))
        (setq kconfig-ref--progress-max (count-lines (point-min) (point-max)))
        (setq kconfig-ref--progress-count 0)
        (setq kconfig-ref--progress-reporter
              (make-progress-reporter
               "Parsing .config ... "
               0 kconfig-ref--progress-max))

        (mapcar (lambda (conf-val)
                  (progress-reporter-update kconfig-ref--progress-reporter
                                            kconfig-ref--progress-count)
                  (kconfig-ref--apply-dot-config conf-val)
                  (cl-incf kconfig-ref--progress-count))
                (split-string (buffer-string) "\n" nil))
        (progress-reporter-done kconfig-ref--progress-reporter))
    (error (format "Failed to parse .config file: %s" err))))

(defun kconfig-ref-find-config-by-name (name)
  "Find a kconfig from the database.
Argument NAME config name."
  (interactive "sEnter the config: ")
  (let ((query-result (kconfig-ref--find-config-by-name name)))
    (if (> (length query-result) 1)
        (progn
          (setq kconfig-ref--choice-list
                (mapcar (lambda (v)
                          (list (format "%s [%s]" (nth 1 v) (nth 5 v)) . (v)))
                        query-result))
          (kconfig-ref-find-config-by-name
           (call-interactively #'kconfig-ref-find-config-by-name-choose)))
      (if (= (length query-result) 1)
          (let ((filp (nth 3 (car query-result)))
                (line (nth 4 (car query-result))))
            (find-file filp)
            (goto-line line))
        (error (format "%s is not found" name))))))

(defun kconfig-ref--find-last-record ()
  "Query a config from the database."
  (kconfig-ref--set-db-filepath)
  (emacsql kconfig-ref-db
           [:select * :from configs :where (= id $s1)]
           (kconfig-ref--find-last-record-id)))

(defun kconfig-ref--is-file-parsed (filepath)
  "Check the FILEPATH (argument) is already parsed before."
  (> (length (emacsql kconfig-ref-db
                      [:select *
                       :from configs
                       :where (= file_loc $s1)]
                      filepath)) 0))

(defun kconfig-ref--parse-kconfig-syntax (syntax &optional filepath line-number)
  "Parse a line of kconfig and do corresponding operation.
Argument SYNTAX a line to parse.
Optional argument FILEPATH Kconfig filepath.
Optional argument LINE-NUMBER line number of the config."
  (let ((keyword (kconfig-ref--get-syntax-type (car (split-string syntax " "))))
        (remains (cdr (split-string syntax " "))))
    (cond ((string= "source" keyword)
           (let ((kconfig-ref--sub-kconfig-filepath
                  (replace-regexp-in-string "\$\(SRCARCH\)" kconfig-ref--default-arch
                                            (expand-file-name
                                             (replace-regexp-in-string "\"" "" (car remains))
                                             (projectile-project-root)) t)))
             (if (or
                  (and kconfig-ref--flag-update-buffer
                       (not (kconfig-ref--is-file-parsed kconfig-ref--sub-kconfig-filepath)))
                  (not kconfig-ref--flag-update-buffer))
                 (kconfig-ref--parse-kconfig-file kconfig-ref--sub-kconfig-filepath)))))
    (cond ((string= "menuconfig" keyword)
           (kconfig-ref--parse-menuconfig (car remains) filepath line-number)))
    (cond ((string= "config" keyword)
           (kconfig-ref--parse-config (car remains) filepath line-number)))))

(defun kconfig-ref--parse-kconfig-file (filepath)
  "Parse a kernel source tree and insert items into the database.
`dirpath' must be valid.
Argument FILEPATH Kconfig file path."
  (if (null filepath)
      (error "Invalid filepath: nil")
    (if (null kconfig-ref--flag-update-buffer)
        (progn
          (cl-incf kconfig-ref--progress-count)
          (progress-reporter-update kconfig-ref--progress-reporter kconfig-ref--progress-count))))
  (let ((line-number 1))
    (with-temp-buffer
      (insert-file-contents filepath)
      (mapcar (lambda (syntax)
                (kconfig-ref--parse-kconfig-syntax syntax filepath line-number)
                (cl-incf line-number))
              (split-string (buffer-string) "\n" nil)))))

(defun kconfig-ref--init-progress ()
  "Initialize kconfig-ref progress counter."
  (let ((default-directory (kconfig-ref-get-projectile-root-dir)))
    (shell-command
     (format "find . -name 'Kconfig' | wc -l")
     "*kconfig-ref*"))

  (let ((number-of-kconfig (string-to-number (with-current-buffer "*kconfig-ref*" (buffer-string)))))
    (setq kconfig-ref--progress-count 0)
    (setq kconfig-ref--progress-max number-of-kconfig)
    (setq kconfig-ref--progress-reporter (make-progress-reporter
                                          "Parsing kconfig in the Linux kernel... "
                                          0 number-of-kconfig))))

(defun kconfig-ref-init-db ()
  "Generate a new database and parse Kconfig(s) in the kernel source tree."
  (interactive)
  (kconfig-ref-create-db t))

(defun kconfig-ref-create-db (&optional force)
  "Create a sqlite db in projectile root directory.
Optional argument FORCE Create the database by force."
  (kconfig-ref--set-db-filepath)
  (kconfig-ref--create-db (kconfig-ref-get-projectile-root-dir) force)
  (kconfig-ref--init-progress)
  (setq kconfig-ref--flag-update-buffer nil)
  (kconfig-ref--parse-kconfig-file
   (expand-file-name "Kconfig" (kconfig-ref-get-projectile-root-dir)))

  (progress-reporter-update kconfig-ref--progress-reporter kconfig-ref--progress-max)
  (progress-reporter-done kconfig-ref--progress-reporter))

(defun kconfig-ref--clean-dirty-records ()
  "Mark dirty bit of records in filepath."
  (emacsql kconfig-ref-db
           [:delete :from configs :where (= dirty 1)]))

(defun kconfig-ref--mark-dirty-by-filepath (filepath)
  "Remove all records with dirty bit.
Argument FILEPATH Kconfig filepath."
  (emacsql kconfig-ref-db
           [:update configs
            :set [(= dirty 1)]
            :where (= file-loc $s1)]
           filepath))

(defun kconfig-ref-parse-current-buffer ()
  "Parse Kconfig (current buffer) and update the database."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (setq kconfig-ref--flag-update-buffer t)
      (cond ((string= (file-name-base filename) "Kconfig")
             (progn
               (kconfig-ref--mark-dirty-by-filepath filename)
               (kconfig-ref--parse-kconfig-file filename)
               (kconfig-ref--clean-dirty-records)))))))

(defvar kconfig-ref--user-directory (expand-file-name "kconfig-ref" user-emacs-directory))

(defun kconfig-ref--init-private-directory ()
  "Initialize private directory for kconfig-ref."
  (ignore-errors
    (mkdir kconfig-ref--user-directory))
  (funcall
   '(lambda (session-name)
      (let ((session-directory-path (expand-file-name session-name kconfig-ref--user-directory)))
        (if (not (file-exists-p (expand-file-name session-name kconfig-ref--user-directory)))
            (mkdir session-directory-path))
        session-directory-path))
   (or (file-remote-p default-directory 'host) "default")))

(provide 'kconfig-ref)
;;; kconfig-ref.el ends here
