;;; sendmail-alias.el --- parse a sendmail-style alias file

;; Copyright (C) 1992 Roland McGrath
;; Copyright (C) 1993, 1995, 2009 Noah S. Friedman

;; Author: Roland McGrath <roland@frob.com>
;;         Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: mail, extensions
;; Created: 1992-04-07

;; $Id: sendmail-alias.el,v 1.7 2009/08/26 02:03:57 friedman Exp $

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file redefines `build-mail-alias' and `build-mail-abbrevs'
;; functions with versions that read sendmail-style alias files (see
;; aliases(5)).  It uses define-mail-{alias/abbrev}, and should work with
;; either mailalias.el or mailabbrev.el.

;; You must load this file AFTER loading mailabbrev or mailalias.
;; One way to insure this always happens so to put the following in your
;; .emacs:
;;
;;        (eval-after-load "mailabbrev"
;;                         '(load "sendmail-alias"))
;;
;;        (eval-after-load "mailalias"
;;                         '(load "sendmail-alias"))

;; There are other caveats to be aware of as well.
;; The function `mail-setup', called by `mail' in sendmail.el, has a call
;; to build-mail-aliases hardwired in.  If you use mailabbrevs instead, you
;; don't need to set mail-aliases also, since that means parsing a possibly
;; large aliases file twice.  The recommended way to use mailabbrevs is to
;; do `(add-hook 'mail-setup-hook 'mail-abbrevs-setup)'.  However, you
;; cannot prevent building mail-aliases by doing anything on
;; mail-setup-hook since that is called after build-mail-aliases has
;; already been called.  You can, however, do the following:
;;
;;        (add-hook 'mail-mode-hook
;;                  (lambda ()
;;                    (and (boundp 'mail-setup-hook)
;;                         (memq 'mail-abbrevs-setup mail-setup-hook)
;;                         (setq mail-aliases nil))))
;;
;; This works because build-mail-aliases only rebuilds mail-aliases if it
;; is set to `t'.
;; This is unpleasantly warty.  mail-setup should be changed to have a hook
;; for building aliases or abbrevs so that only one or the other is done.

;;; Code:

(require 'sendmail)

;; This exists in emacs 19.29 and later.
;; For earlier versions, just define it here.
(defvar mail-personal-alias-file "~/etc/aliases"
  "*If non-nil, the name of the user's personal mail alias file.
This file typically should be in same format as the `.mailrc' file used by
the `Mail' or `mailx' program, but that is not strictly necessary (it
depends on the underlying parsing agent).
This file need not actually exist.")

(defun sendmail-alias-build-table (defn tbl &optional file)
  (or file (setq file mail-personal-alias-file))
  (setq file (expand-file-name file))
  (message "Reading aliases from %s..." file)
  (let ((buf (generate-new-buffer " *aliases*")))
    (save-excursion
      (set-buffer buf)
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^#\n][^: \t]*\\)[ \t]*:\\s *\
\\([^\n]+\\(\n[ \t]+.*\\)*\\)"
                                nil t)
        (funcall defn
                 (buffer-substring (match-beginning 1) (match-end 1))
                 (buffer-substring (match-beginning 2) (match-end 2)))))
    (kill-buffer buf))
  ;; Read in :include: files and replace.
  (cond ((vectorp (symbol-value tbl))
         (mapatoms (lambda (sym)
                     ;; abbrev tables have an interned symbol named ""
                     ;; (i.e. the empty string) with no value.  Various
                     ;; table parameters are stored on that symbol's
                     ;; property list but we're not interested in those.
                     ;; So skip symbols whose value is nil.
                     (when (symbol-value sym)
                       (sendmail-alias-expand-include sym)))
                   (symbol-value tbl)))
        (t
         (mapc 'sendmail-alias-expand-include (symbol-value tbl))))
  (message "Reading aliases from %s...done" file))

(defun sendmail-alias-expand-include (alias)
  (let ((expansion (if (symbolp alias)
		       (symbol-value alias)
		     ;; Association (NAME . VALUE).
		     (cdr alias)))
	(tmpbuf (generate-new-buffer " :include: text"))
	(curbuf (current-buffer)))
    (unwind-protect
	(while (string-match "\\s *:include:\\(.+\\)\\s *" expansion)
	  (let* ((file (expand-file-name (substring expansion
						    (match-beginning 1)
						    (match-end 1))))
		 (filebuf (get-file-buffer file))
		 (buf (find-file-noselect file)))
	    (unwind-protect
		(progn
		  (set-buffer buf)
		  (save-restriction
		    (widen)
		    (let ((beg (point-min))
			  (end (point-max)))
		      (set-buffer tmpbuf)
		      (erase-buffer)
		      (insert-buffer-substring buf beg end))
		    (goto-char (point-min))
		    (while (re-search-forward "^\\(.+[^ \t,]\\)[ \t]*\n" nil t)
		      (if (eobp)
			  (backward-delete-char 1)
			(replace-match "\\1,\n" t)))
		    (goto-char (point-min))
		    (replace-string "\n" " ")
		    (setq expansion (buffer-substring (point-min)
						      (point-max)))))
	      (or filebuf
		  (kill-buffer buf))
	      (set-buffer curbuf))))
      (kill-buffer tmpbuf))
    (if (symbolp alias)
	(set alias expansion)
      (setcdr alias expansion))))

;;; This is called by mailalias.el.
(defadvice build-mail-aliases (around sendmail-alias activate)
  "Use `sendmail-alias-build-table' to parse the aliases file."
  (sendmail-alias-build-table
   'define-mail-alias 'mail-aliases (ad-get-arg 0)))

;; This is called by mailabbrev.el
;; Note that in order for mail-mode to use mailabbrev, you have to do:
;;    (add-hook 'mail-setup-hook 'mail-abbrevs-setup)
(defadvice build-mail-abbrevs (around sendmail-alias activate)
  "Use `sendmail-alias-build-table' to parse the aliases file."
  ;; Initialize mail-abbrevs so that we are not called recursively by
  ;; define-mail-abbrev.
  (unless (vectorp mail-abbrevs)
    (setq mail-abbrevs nil)
    (define-abbrev-table 'mail-abbrevs nil))

  (sendmail-alias-build-table
   'define-mail-abbrev 'mail-abbrevs (ad-get-arg 0)))


(provide 'sendmail-alias)

;;; sendmail-alias.el ends here.
