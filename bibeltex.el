;;; bibeltex.el --- BibTeX-like implementation for org-mode
;;
;; Copyright (C) 2013 Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;;
;; Authors: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Keywords: org
;;
;; This file is NOT part of GNU Emacs.
;;
;; BibELTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; BibELTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with BibELTeX.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README.org

;;; Code:

(require 'ox)
(require 'cl-lib)
(require 'org-bibtex)

(defgroup bibeltex nil
  "Simple BibTeX-like implementation for org-mode in emacs lisp."
  :tag "BibELTeX"
  :prefix "bibeltex-"
  :group 'org)

(defcustom bibeltex-ignore-backends nil
  "List of backends to ignore."
  :group 'bibeltex
  :type '(list (symbol :tag "Name of back end.")))

(defcustom bibeltex-use-bibtex-in-latex t
  "Use BibTeX instead of BibELTeX for LaTeX export."
  :group 'bibeltex
  :type 'boolean)

(defcustom bibeltex-sort-function nil
  "Sort entries."
  :group 'bibeltex
  :type '(choice (function :tag "Sort function")
                 (const :tag "No sorting" nil)))

(defcustom bibeltex-export-all t
  "Export all entries."
  :group 'bibeltex
  :type 'boolean)

(defcustom bibeltex-use-style 'default
  "Use a \"style\" instead of `org-bibtex-write' to format entries.
See `bibeltex-style-alist' for potential styles."
  :group 'bibeltex
  :type 'symbol)

(defcustom bibeltex-style-alist '((default . bibeltex-style-default))
  "Map style name to style variable."
  :group 'bibeltex
  :type '(list (cons (symbol :tag "Style name")
                     (variable :tag "Style variable"))))

(defcustom bibeltex-format-cite "^{[[#\\1][\\1]]}"
  "How to format \\cite.
\\1 is replaced with the key name."
  :group 'bibeltex
  :type 'string)

(defun bibeltex--use-org-bibtex ()
  "Use `org-bibtex-write' to insert bibtex data."
  (insert "* Bibliography\n")
  (let ((pt (point)))
    (insert "** bib\n") ;; force `org-bibtex-write' to insert level two headings
    (dotimes (_ (length org-bibtex-entries))
      (save-excursion
        (org-bibtex-write))
      (re-search-forward org-property-end-re)
      (open-line 1) (forward-char 1))
    (goto-char pt) (kill-line)))

(defun bibeltex--handle-bibtex (&optional list-of-keys)
  "Handle the bibtex part.
If LIST-OF-KEYS is non-nil and `bibeltex-export-all' nil then only insert
entries from the list."
  (when (and (not bibeltex-export-all) list-of-keys)
    (setq org-bibtex-entries
          (cl-delete-if (lambda (entry)
                          (not (member (cdr (assq :key entry)) list-of-keys)))
                        org-bibtex-entries)))
  (when bibeltex-sort-function
    (setq org-bibtex-entries
          (sort org-bibtex-entries #'bibeltex-sort-function)))

  (if bibeltex-use-style
      (bibeltex--use-style bibeltex-use-style)
    (bibeltex--use-org-bibtex)))

(defun bibeltex-bib-file-name (file)
  "Convert FILE to a proper file name."
  (if (string-match-p "\\.bib\\'" file)
      file
    (concat file ".bib")))

(defun bibeltex--filter (backend)
  "Replace \\cite{x} with links and handle bibliography."
  (unless (memq backend bibeltex-ignore-backends)
    (let (list-of-keys)
      (unless (and (eq backend 'latex)
                   bibeltex-use-bibtex-in-latex)
        ;; TODO handle multiple cites
        (while (re-search-forward "\\\\cite{\\(.*?\\)}" nil 'noerror)
          (push (match-string 1) list-of-keys)
          (replace-match bibeltex-format-cite)))
      (let (org-bibtex-entries)
        (goto-char (point-min))
        (while (re-search-forward "#\\+BIBLIOGRAPHY:[ \t]+\\([[:alnum:]-_.]+\\)[ \t]+\\([[:alnum:]]+\\)[ \t]*\\([[:alnum:]-:]+\\)?" nil 'noerror)
          (let ((file (match-string 1))
                (style (match-string 2))
                (options (match-string 3)))
            (beginning-of-line) (kill-line)
            (if (and (eq backend 'latex)
                     bibeltex-use-bibtex-in-latex)
                (insert (format "\\bibliographystyle{%s}\n\\bibliography{%s}\n"
                                style file)) ;; TODO biblatex support
              (org-bibtex-read-file (bibeltex-bib-file-name file))))
          (unless (and (eq backend 'latex)
                       bibeltex-use-bibtex-in-latex)
            (bibeltex--handle-bibtex list-of-keys)))))))

(add-hook 'org-export-before-parsing-hook
          #'bibeltex--filter)


;;; Style

;; Code inspired by reftex-cite.el

(defun bibeltex--get-field (field entry)
  "Return FIELD from ENTRY or empty string."
  (or (cdr (assq field entry)) ""))

(defun bibeltex--get-names (field entry)
  "Return list of names for FIELD in ENTRY."
  (let ((names (bibeltex--get-field field entry)))
    (setq names (replace-regexp-in-string "\\b[eE]t[ \t]+al\\."
                                          "and /et al./" names))
    (split-string names "[ \t]*\\band\\b[ \t]*" 'omit-nulls)))

(defconst bibeltex--format-map
  '((?l . :key)
    (?n . :number)
    (?v . :volume)
    (?s . :school)
    (?u . :publisher)
    (?r . :address)
    (?U . :url)
    (?t . :title)
    (?p . :pages)
    (?o . :organization)
    (?N . :note)
    (?j . :journal)
    (?i . :institution)
    (?h . :howpublished)
    (?b . :booktitle)
    (?d . :edition)
    (?D . :doi)
    (?c . :chapter)
    (?m . :month)
    (?y . :year))
  "Map format spec to field name.")

(defun bibeltex--format-names (field entry &optional num)
  "Format NUM names for FIELD in ENTRY.
If NUM is not a number format all names.
If names are cut off they are replaced by \"et al.\""
  (let ((names (bibeltex--get-names field entry))
        str)
    (when (numberp num)
      (setq names (org-sublist names 1 num)))
    (setq str
          (mapconcat #'identity
                     names
                     ", "))
    (if (and (numberp num) (< num (length names)))
        (concat str ", /et al./")
      str)))

(defun bibeltex--format-entry (format entry)
  "Format ENTRY according to FORMAT."
  (while (string-match "%\\([0-9]*\\)\\([a-zA-Z%]\\)" format)
    (let ((num (string-to-number (match-string 1 format)))
          (spec (string-to-char (match-string 2 format))))
      (setq format
            (replace-match
             (save-match-data
               (cl-case spec
                 (?% "%") ;; %%
                 (?a (bibeltex--format-names :author entry (when (/= num 0) num)))
                 (?A (car (bibeltex--get-names :author entry)))
                 (?e (bibeltex--format-names :editor entry num))
                 (?E (car (bibeltex--get-names :editor entry)))
                 (?P (car (split-string (bibeltex--get-field :pages entry)
                                        "[- .]+")))
                 (otherwise ;; use `bibeltex-format-map'
                  (let ((field (cdr (assq spec bibeltex--format-map))))
                    (unless field
                      (error "Invalid format string: %s"
                             (match-string 0 format)))
                    (bibeltex--get-field field entry)))))
             'fixed-case 'literal format))))
  format)

(defgroup bibeltex-styles nil
  "Styles for BibELTeX."
  :group 'bibeltex
  :prefix "bibeltex-style")

(defcustom bibeltex-style-default
  '((article . "** [%l] %2a, %y: %t\n:PROPERTIES:\n:CUSTOM_ID: %l\n:END:\n\n- %y %m\n- [[%U]]\n- DOI: [[doi://%D][%D]]\n\n")
    (t . "** [%l] %2a, %y: %t\n:PROPERTIES:\n:CUSTOM_ID: %l\n:END:\n\n"))
  "Default style."
  :group 'bibeltex-styles)

(defun bibeltex--get-style (style)
  "Get STYLE format instructions."
  (let ((style-var (cdr (assq style bibeltex-style-alist))))
    (unless style-var
      (error "Unknown style `%s' not found in `bibeltex-style-alist'" style))
    (symbol-value style-var)))

(defun bibeltex--style-handle-entry (entry style)
  "Write ENTRY according to STYLE."
  (let* ((type-tmp (cdr (assq :type entry)))
         (type (intern (downcase type-tmp)))
         (format (cdr (or (assq type style)
                          (assq t style)))))
    (insert (bibeltex--format-entry format entry))
    (unless (bolp) ;; Make sure we end with a newline
      (insert "\n"))))

(defun bibeltex--use-style (style)
  "Use STYLE to format `org-bibtex-entries'."
  (setq style (bibeltex--get-style style))
  (insert "\n* Bibliography\n")
  (dolist (entry org-bibtex-entries)
    (bibeltex--style-handle-entry entry style)))

(provide 'bibeltex)

;;; bibeltex.el ends here
