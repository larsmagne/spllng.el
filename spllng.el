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
  "You're a spell checker and a copy editor.
You will be given a text that is a possibly an HTML fragment, but
can be any text format.  Spell-check this text.

Check for the meaning of the sentences, whether words have been
substituted for other words.  Check for noun/verb agreement etc.

Make sure that you spell-check the entire text.

Do not change slang or abbreviations like \"readin'\" or
\"mainstreamey\".  Use British, not American spelling.

Return an array of things to be changed in JSON format, looking
like this:

[
 [\"foo bzr zot\", \"foo bar zot\"],
 ...
]

The first field should match text to be replaced.  The match
field should be large enough context and avoid ambiguity.

The second field should be the corrected phrase.

Be very careful about making sure that the JSON is valid (no
trailing or missing commas, all strings properly terminated, all
delimiters properly matched up). Return just the JSON.  Don't
wrap the JSON in \"```\" characters.

The next line starts the text to spell-check: "
  "The prompt to send over to the LLM.  Should be adjusted to your needs.")

;;; Prompt partly adapted from
;;; https://codeberg.org/sachac/learn-lang/src/branch/main/learn-lang-flycheck-gptel.el

(defvar spllng-provider 'claude
  "Which LLM to ask about spelling.
See query-assistant.el for valid values.")

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
    (let* ((region (spllng--massage-region start end))
	   (new (spllng--check region))
	   (json (mapcar (lambda (a) (cl-coerce a 'list))
			 (spllng--parse-json new))))
      (setq j json)
      (if (equal new "[]")
	  (progn
	    (message "No changes")
	    (goto-char point))
	(undo-boundary)
	(save-restriction
	  (narrow-to-region start end)
	  ;; Do the replacements.
	  (cl-loop
	   for (orig-phrase replacement-phrase) in json
	   when (progn
		  (goto-char (point-min))
		  (search-forward orig-phrase nil t))
	   do
	   (goto-char (match-beginning 0))
	   (cl-destructuring-bind (prefix orig replacement _suffix)
	       (spllng--string-difference orig-phrase replacement-phrase)
	     (forward-char (length prefix))
	     (let ((start (point)))
	       (delete-region start (+ start (length orig)))
	       ;; Tag up the text so that commands can interact with it.
	       (insert
		(propertize replacement
			    'face 'error
			    'spllng-changed t
			    'keymap spllng-word-map
			    'state 'changed
			    'start (set-marker (make-marker) start)
			    'original orig
			    'changed replacement))
	       (put-text-property (match-beginning 0)
				  (+ start (length replacement))
				  'end
				  (set-marker (make-marker)
					      (+ start
						 (length replacement)))))))
	  (goto-char (point-min))
	  (run-hooks 'spllng-after-change-hook)
	  (spllng-next-word)
	  (message "Spell-checking...Done"))))))

(defun spllng--parse-json (string)
  (with-temp-buffer
    (insert string)
    ;; The LLM somehow likes wrapping the json in "```", so check and
    ;; remove that.
    (goto-char (point-min))
    (when (looking-at "```\\(json\\)?")
      (replace-match "")
      (goto-char (point-max))
      (and (re-search-backward "```" nil t)
	   (replace-match "")))
    (goto-char (point-min))
    (condition-case _err
	(json-parse-buffer)
      (error
       (error "Couldn't parse JSON: %s" (buffer-string))))))

(defun spllng-buffer ()
  "Replace the current buffer with a spell-checked version."
  (interactive)
  (spllng-region (point-min) (point-max)))

(defun spllng--string-difference (s1 s2)
  "Return four strings.
The common prefix, the s1 diff, the common suffix, the s2 diff."
  (let* ((prefix (spllng--string-prefix s1 s2))
	 (suffix (reverse
		  (spllng--string-prefix (reverse s1) (reverse s2)))))
    (list prefix
	  (substring s1 (length prefix)
		     (- (length s1) (length suffix)))
	  (substring s2 (length prefix)
		     (- (length s2) (length suffix)))
	  suffix)))

(defun spllng--string-prefix (s1 s2)
  (cl-loop with chop
	   for i from 0
	   for c1 across s1
	   for c2 across s2
	   while (= c1 c2)
	   ;; We want a word, so chop after a space.
	   when (and (> i 0)
		     (not (= c1 ?\s))
		     (= (elt s1 (1- i)) ?\s))
	   do (setq chop i)
	   finally (cl-return (substring s1 0 (or chop 0)))))

(defun spllng--massage-region (start end)
  "Return the pertinent text in the buffer between START and END.
Filter out pure-HTML constructs to get the token count and
thereby the amount of LLM time used down."
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf start end)
      (goto-char (point-min))
      ;; The most egregious thing in Wordpress posts is how images are
      ;; included -- there's a lot of text in those links.  So remove
      ;; and stash them.  Also stash text from <blockquote>s --
      ;; they're presumably quoted bits that you don't want to
      ;; spellcheck.
      (while (re-search-forward "<a [^>]+?><img [^>]+?></a>\\|<img [^>]+?>\\|<blockquote>[^z-a]+?</blockquote>" nil t)
	(replace-match "" t t))
      (buffer-string))))

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

(provide 'spllng)

;;; spllng.el ends here
