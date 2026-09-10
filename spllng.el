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

;; This minor mode uses the `query-assistant.el' package to query LLMs
;; to do spelling/grammar checks.

;; https://github.com/larsmagne/query-assistant.el

;;; Code:

(require 'cl-lib)
(require 'query-assistant)

(defvar spllng-prompt
  "You're a copy editor.
Respond with the spell-checked text only.

The text you're given is an HTML fragment; keep the same HTML strucure.
Do not add any additional HTML structures.

If you don't make any changes, return ':no-change' only.

For every changed word in the text, transform that word to
(spllng-changed :orig \"...\" :changed \"...\") inside the text,
and return the changed text.  (If there are embedded quote
characters in the strings, quote them with a backslash -- return
:orig \"WHAT\\\"S\" if the string is \"WHAT\"S\".)

Do not change slang or abbreviations like \"readin'\" or
\"mainstreamey\".  Use British, not American spelling.

Check for the meaning of the sentences, whether words have been
substituted for other words.  Check for noun/verb agreement.

Make sure you're not marking something as changed when you
haven't changed anything, but if you have changed something, make
sure that you mark your changes.  Do not include anything else in
your answer except the corrected text, even if there is no text
included, or there's nothing to be changed.  Preserve white
space.

Make sure that you spell-check the entire text.  Ensure that you
return the same number of lines as you got -- don't delete lines
that you don't think is HTML.  (In particular, don't remove
header lines.)

The next line starts the text to spell-check: "
  "The promt to send over to the LLM.  Should be adjusted to your needs.")

(defvar spllng-provider 'claude
  "Which LLM to ask about spelling.
See query-assistant.el for valid values.")

(defvar spllng-debug nil
  "If non-nil, debug the output from the LLM on errors.")

(defvar spllng-after-change-hook nil
  "Hook run after changing a portion of the buffer.
It's called narrowed to the changed part with point at the start.")

(define-minor-mode spllng-mode
  "Minor mode to spellcheck the buffer.")

(defvar-keymap spllng-mode-map
  "C-c C-e" #'spllng-region
  "C-c C-f" #'spllng-buffer)

(defvar-keymap spllng-word-map
  "C-c C-n" #'spllng-next-word
  "C-c C-p" #'spllng-previous-word
  "TAB" #'spllng-toggle-word)

(defalias 'spllng 'spllng-region)
(defun spllng-region (start end)
  "Replace the region with a spell-checked region.
If something is replaced, you will be positioned on the first changed word.
You can toggle the original/fixed with with the \\<spllng-word-map>\\[spllng-toggle-word] command.

Use \\[spllng-next-word] to go to the next fixed word and
\\[spllng-previous-word] to go to previous fixed word."
  (interactive "r")
  (let ((point (point-marker)))
    ;; Don't send over any leading/trailing white space, because the
    ;; LLM won't preserve that part.  So adjust start/end.
    (goto-char end)
    (skip-chars-backward "\n\t ")
    (setq end (point))
    (goto-char start)
    (skip-chars-forward "\n\t ")
    (setq start (point))
    (let* ((region (spllng--massage-region start end))
	   (new (spllng--check (car region))))
      (if (equal new ":no-change")
	  (progn
	    (message "No changes")
	    (goto-char point))
	;; Do some sanity checks on the returned data to see whether
	;; the LLM has gone off the rails.
	(when-let ((err (spllng--check-response (car region) new)))
	  (when spllng-debug
	    (spllng--display-difference (car region) new))
	  (error "The LLM has apparently given a bad response this time; try again: %s"
		 err))
	(undo-boundary)
	(save-restriction
	  (narrow-to-region start end)
	  (delete-region (point-min) (point-max))
	  (insert new)
	  (goto-char (point-min))
	  ;; We've instructed the LLM to mark up spellchecked words
	  ;; like this:
	  ;;
	  ;; Some wrng text.
	  ;; ->
	  ;; Some (spllng-changed :orig "wrng" :changed "wrong") text.
	  ;;
	  ;; That's probably more verbose than needed, but eh,
	  ;; whatevs.  If makes it easy on this side when dealing with
	  ;; strings that have embedded quote marks.
	  (while (re-search-forward "(spllng-changed :orig " nil t)
	    (goto-char (match-beginning 0))
	    (let ((start (point))
		  (form (read (current-buffer)))
		  (end (point)))
	      (let ((orig (plist-get (cdr form) :orig))
		    (changed (plist-get (cdr form) :changed)))
		;; Sometimes (by mistake) the LLM says that it's
		;; changed something, but it hasn't.  Filter those
		;; out.
		(unless (equal orig changed)
		  (delete-region start end)
		  ;; Tag up the text so that commands can interact with it.
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
						    (length changed))))))))
	  (spllng--restore-massage (cdr region))
	  (goto-char (point-min))
	  (run-hooks 'spllng-after-change-hook)
	  (spllng-next-word)
	  (message "Spell-checking...Done"))))))

(defun spllng-buffer ()
  "Replace the current buffer with a spell-checked version."
  (interactive)
  (spllng-region (point-min) (point-max)))

(defun spllng--massage-region (start end)
  "Return the pertinent text in the buffer between START and END.
Filter out pure-HTML constructs to get the token count and
thereby the amount of LLM time used down.

Return a tuple of FILTERED-BUFFER-TEXT and HTML-TABLE."
  (let ((buf (current-buffer))
	(table (make-hash-table :test #'equal))
	(i 1))
    (with-temp-buffer
      (insert-buffer-substring buf start end)
      (goto-char (point-min))
      ;; The most egregious thing in Wordpress posts is how images are
      ;; included -- there's a lot of text in those links.  So remove
      ;; and stash them.  Also stash text from <blockquote>s --
      ;; they're presumably quoted bits that you don't want to
      ;; spellcheck.
      (while (re-search-forward "<a [^>]+?><img [^>]+?></a>\\|<img [^>]+?>\\|<blockquote>[^z-a]+?</blockquote>" nil t)
	(setf (gethash (format "%d" i) table)
	      (buffer-substring (match-beginning 0) (match-end 0)))
	(replace-match (format "<div id=\"sp-%d\"></div>" i) t t)
	(cl-incf i))
      (cons (buffer-string) table))))

(defun spllng--restore-massage (table)
  "Restore placeholders."
  (goto-char (point-min))
  (while (re-search-forward "<div id=\"sp-\\([0-9]+\\)\"></div>" nil t)
    (replace-match (gethash (match-string 1) table) t t)))

(defun spllng--check (line)
  (message "Spell-checking...")
  (query-assistant spllng-provider (concat spllng-prompt "\n" line)))

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

(defun spllng--check-response (orig new)
  "Return nil for OK and the error message if there's an error."
  (let ((ostats (spllng--text-stats orig))
	(nstats (spllng--text-stats new)))
    (cond
     ((< (length new) (length orig))
      ;; This should never happen -- I mean, a fixed word may be shorter
      ;; than the original word, but since it also includes the original
      ;; text in the response, this would be an error.
      "LLM output shorter than the original text")
     ((< (plist-get nstats :lines) (plist-get ostats :lines))
      ;; Perhaps the LLM decided to concatenate some lines.
      "LLM output has fewer lines than the original text")
     ((not (= (plist-get nstats :html) (plist-get ostats :html)))
      ;; The number of HTML elements should remain exactly the same --
      ;; nothing added, nothing removed.
      (format "LLM output has a different number of HTML elements than the original version: %d (orig) vs %d (new)"
	      (plist-get ostats :ostats)
	      (plist-get nstats :ostats))))))

(defun spllng--display-difference (orig new)
  (let ((oname (make-temp-name "/tmp/spllng1"))
	(nname (make-temp-name "/tmp/spllng2")))
    (unwind-protect
	(progn
	  (write-region orig nil oname)
	  (write-region new nil nname)
	  (diff oname nname))
      (when (file-exists-p oname)
	(delete-file oname))
      (when (file-exists-p nname)
	(delete-file nname)))))

(defun spllng--text-stats (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((lines 0)
	  (html 0))
      ;; Count lines.
      (while (not (eobp))
	(cl-incf lines)
	(forward-line 1))
      ;; Count HTML.
      (while (re-search-forward "<[a-zA-Z]+\\b" nil t)
	(cl-incf html))
      (list :lines lines
	    :html html))))

(provide 'spllng)

;;; spllng.el ends here
