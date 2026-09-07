;;; spllng.el --- Check spelling -*- lexical-binding: t -*-

;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress, blogs

;; spllng is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; spllng is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar spllng-after-change-hook 'ewp--hide-links
  "Hook run after changing a portion of the buffer.
It's called narrowed to the changed part with point at the start.")

(defvar spllng-prompt
  "You're a copy editor.  Respond with the spell-checked text only.  The text is an HTML fragment; keep the same HTML strucure.  Do not add any additional HTML structures.  If you don't make any changes, return ':no-change' only.  For every changed word in the text, transform that word to (spllng-changed :orig \"...\" :changed \"...\") inside the text, and return the changed text.  (If there are embedded quotes in the strings, quote them with a backslash.)  Do not suggest grammar changes.  Do not change slang or abbreviations like \"readin'\" or \"mainstreamey\".  Use British, not American spelling.  Check for the meaning of the sentences, whether words have been substituted for other words.  Check that noun/verb plurarity agrees.  Make sure you're not marking something as changed when you haven't changed anything, but if you have changed something, make sure that you mark your changes.  Do not include anything else in your answer except the corrected text, even if there is no text included, or there's nothing to be changed.  Preserve white space.  The next line starts the text to spell-check: ")

(define-minor-mode spllng-mode
  "Minor mode to spellcheck the buffer.")

(defvar-keymap spllng-mode-map
  "C-c C-e" #'spllng)

(defvar-keymap spllng-word-map
  "C-c C-n" #'spllng-next-word
  "C-c C-p" #'spllng-previous-word
  "TAB" #'spllng-toggle-word)

(defun spllng (start end)
  "Replace the region with a spell-checked region."
  (interactive "r")
  (let ((point (point-marker)))
    (goto-char end)
    (skip-chars-backward "\n\t ")
    (setq end (point))
    (message "Querying...")
    (let ((new (spllng--check (buffer-substring start end))))
      (if (equal new ":no-change")
	  (progn
	    (message "No changes")
	    (goto-char point))
	(undo-boundary)
	(save-restriction
	  (narrow-to-region start end)
	  (delete-region (point-min) (point-max))
	  (insert new)
	  (goto-char (point-min))
	  (while (re-search-forward "(spllng-changed :orig " nil t)
	    (goto-char (match-beginning 0))
	    (let ((start (point))
		  (form (read (current-buffer)))
		  (end (point)))
	      (let ((orig (plist-get (cdr form) :orig))
		    (changed (plist-get (cdr form) :changed)))
		(delete-region start end)
		(insert
		 (propertize changed
			     'face 'error
			     'spllng-changed t
			     'keymap spllng-word-map
			     'state 'changed
			     'start (set-marker (make-marker) start)
			     'original orig
			     'changed changed))
		(put-text-property (match-beginning 0)
				   (+ start (length changed))
				   'end
				   (set-marker (make-marker)
					       (+ start
						  (length changed)))))))
	  (goto-char (point-min))
	  (run-hooks 'spllng-after-change-hook)
	  (spllng-next-word)
	  (message "Querying...Fixed"))))))

(defun spllng--check (line)
  (query-assistant 'claude (concat spllng-prompt "\n" line)))

(defun spllng-toggle-word ()
  "Toggle the fixed word under point."
  (interactive)
  (let* ((props (text-properties-at (point)))
	 (new (if (eq (plist-get props 'state) 'changed)
		  (plist-get props 'original)
		(plist-get props 'changed)))
	 (start (plist-get props 'start)))
    (delete-region start (plist-get props 'end))
    (insert new)
    (setf (plist-get props 'state)
	  (if (eq (plist-get props 'state) 'changed)
	      'original
	    'changed))
    (setf (plist-get props 'start) (set-marker (make-marker) start))
    (setf (plist-get props 'end) (set-marker (make-marker)
					     (+ start (length new))))
    (add-text-properties start (+ start (length new)) props)
    (goto-char start)
    (message "Now showing %s phrase" (plist-get props 'state))))

(defun spllng-next-word ()
  "Go to the next changed word."
  (interactive)
  (if-let ((match (text-property-search-forward 'spllng-changed nil nil t)))
      (goto-char (prop-match-beginning match))
    (message "No next word")))

(defun spllng-previous-word ()
  "Go to the previous changed word."
  (interactive)
  (unless (text-property-search-backward 'spllng-changed nil nil t)
    (message "No previous word")))

(provide 'spllng)

;;; spllng.el ends here
