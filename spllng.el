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
  "You're a copy editor.  Respond with the spell-checked text only.  The text is an HTML fragment; keep the same HTML strucure.  If you don't make any changes, return ':no-change' only.  For every changed word, enclose the changed word with <changed orig='...'>...</changed>, where '...' is the word/phrase that was changed.  Do not suggest grammar changes.  Do not change slang or abbreviations like \"readin'\" or \"mainstreamey\".  Use British, not American spelling.  Check for the meaning of the sentences, whether words have been substituted for other words.  Check that noun/verb plurarity agrees.  Make sure you're not marking something as changed when you haven't changed anything, but if you have changed something, make sure that you mark your changes.  Do not include anything else in your answer except the corrected text, even if there is no text included, or there's nothing to be changed.  Preserve white space.  The next line starts the text to spell-check: ")

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
	  (message "No changes")
	(undo-boundary)
	(save-restriction
	  (narrow-to-region start end)
	  (delete-region (point-min) (point-max))
	  (insert new)
	  (goto-char (point-min))
	  (while (re-search-forward "<changed orig='\\([^']+\\)'>\\(.*?\\)</changed>" nil t)
	    (let ((orig (match-string 1))
		  (changed (match-string 2)))
	      (replace-match
	       (propertize changed
			   'face 'error
			   'spllng-changed t
			   'keymap spllng-word-map
			   'state 'changed
			   'start (set-marker (make-marker) (match-beginning 0))
			   'original orig
			   'changed changed)
	       t t)
	      (put-text-property (match-beginning 0)
				 (+ (match-beginning 0) (length changed))
				 'end
				 (set-marker (make-marker)
					     (+ (match-beginning 0)
						(length changed))))))
	  (goto-char (point-min))
	  (run-hooks 'spllng-after-change-hook)
	  (message "Querying...Fixed"))))
    (goto-char point)))

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
  (if (text-property-search-forward 'spllng-changed nil nil t)
      (text-property-search-backward 'spllng-changed)
    (message "No next word")))

(defun spllng-previous-word ()
  "Go to the previous changed word."
  (interactive)
  (unless (text-property-search-backward 'spllng-changed nil nil t)
    (message "No previous word")))

(provide 'spllng)

;;; spllng.el ends here
