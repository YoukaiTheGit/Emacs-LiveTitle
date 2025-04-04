;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; sub.el
;;;
;;; Functions for manipulating subtitle source files, which are a
;;; homebrew format intended as an intermediate format for segmenting
;;; text files into slides of subtitles, which has a minimal amount of markup,
;;; but still represent the structure.
;;; A subtitle file is a plain text file, with one line of text per subtitle line,
;;; each screen of subtitles is separated by a line containing '--'
(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'whitespace)

(defun goto-slide-at-pos (p)
  (goto-char p))

(defconst sub-slide-separator "--\\|==")

(defvar slide-change-hook nil
  "Hook function(s) to run when the slide changes.")

(defun next-slide ()
  "Moves to beginning of next slide.  Returns NIL if no next"
  (interactive)
  (moveto-next-slide)
  (run-hooks 'slide-change-hook))

(defun moveto-next-slide ()
  (goto-char (point-at-eol))
  (when (search-forward-regexp sub-slide-separator nil t)
    (goto-slide-at-pos (point-at-bol))))

(defun beginning-of-slide ()
  (interactive)
  (goto-char (point-at-eol))
  (search-backward-regexp sub-slide-separator)
  (goto-char (point-at-bol)))

(defun prev-slide ()
  (interactive)
  (moveto-prev-slide)
  (run-hooks 'slide-change-hook))

(defun moveto-prev-slide ()
  (goto-char (point-at-eol))
  (search-backward-regexp sub-slide-separator)
  (search-backward-regexp sub-slide-separator)
  (goto-slide-at-pos (point-at-bol)))

;; Our internal representation for a slide is a list of strings.
;; The first string is the "comment" for the slide.  In text form,
;; it is the content just after the after the '--' marker.
;; The remaining strings are the content lines of the slide, minus the EOL character.

(cl-defstruct slide
  ""
  (type 'content :documentation "whether the slide contains content, or a template")
  (comment "" :documentation "The comment of the slide" )
  (lines '() :documentation "List of the lines as strings")
  (template "" :documentation "The name of the HTML formatting template to use")
  begin
  end
  )
    
;; A slide looks like this:
;; --[comment] or ==[comment]
;; line1
;; line2
;; ...
;; (until the next slide)

(defun slide-contents-to-slide (slide)
  (let* ((raw (split-string slide "\n" nil))
         (metadata (slide-parse-comment (substring (car raw) 2))))
    (make-slide :comment (car metadata)
                :type (if (s-starts-with-p "==" (car raw)) 'template 'content)
                :template (or (car (cdr metadata)) "")
                ;; last elt is empty string before next slide separator
                :lines (reverse (cdr (reverse (cdr raw)))))
    ))

(defun slide-parse-comment (comment)
  "Parses metadata out of a slide commment, returning (COMMENT CLASS) where CLASS is anything inside {}, and COMMENT is the rest of the text"
  (when (string-match "^\\([^\\{]*\\)\\({\\([^}]+\\)}\\)?\\(.*\\)" comment)
    (list (concat (match-string 1 comment)
                  (or (match-string 4 comment) ""))
          (match-string 3 comment))))

(defun slide-get-slide-at-point ()
  "Returns the current slide contaiing point to a slide struct"
  (save-excursion
    (beginning-of-slide)
    (let ((bos (point)))
      (moveto-next-slide)
      (let* ((text (buffer-substring-no-properties bos (point)))
	     (slide (slide-contents-to-slide text)))
        (setf (slide-begin slide) bos)
        (setf (slide-end slide) (point))
        slide))))

(defun kill-slide-as-list ()
  "Removes the current slide from the buffer, returning its contents as a slide,
and leaving point at the beginning of the following slide"
  (beginning-of-slide)
  (let ((bos (point)))
    (moveto-next-slide)
    (let ((slide (buffer-substring-no-properties bos (point))))
      (kill-region bos (point))
      (slide-contents-to-slide slide))))

(defun slide-insert (slide)
  "Inserts a new slides whose lines are the elements of LINES"
  (insert "--" (slide-comment slide) "\n")
  (mapc #'(lambda (line)
	    (insert line "\n"))
	(slide-lines slide)))

(defun merge-slides ()
  "merges (appends) the corresponding lines of the next slide to thhe current slide, removing the next slide, and leaving point in the current slide.  E.g.,
--
L1
L2
--
L3
L4
--

becomes

--
L1 L3
L2 L4
--"
  (interactive)
  (merge-slides-with-delim " "))

(defun merge-names ()
  (interactive)
  (merge-slides-with-delim ": "))

(defun merge-slides-with-delim (delim)
  "merges the corresponding lines of the current & next slides, with DELIM between them, and leaves point in the resulting slide."
  (interactive)
  (let* ((slide1 (kill-slide-as-list))
	 (slide2 (kill-slide-as-list)))
    (when (/= (length (slide-lines slide1)) (length (slide-lines slide2)))
      (error "Slides are not the same length"))
    (setf (slide-lines slide1)
          (mapcar* #'(lambda (line1 line2)
		       (format "%s%s%s" line1 delim line2))
	           (slide-lines slide1)
	           (slide-lines slide2)))
    (slide-insert slide1))
  (moveto-prev-slide))

(defun interleave-slides ()
  "Replaces the the current and previous slides with a single slide containing the lines of both previous & current interleaved.  Leaves point in the slide after the newly-created slide."
  (interactive)
  (moveto-prev-slide)
  (let* ((slide1 (kill-slide-as-list))
	 (slide2 (kill-slide-as-list)))
    (when (/= (length (slide-lines slide1)) (length (slide-lines slide2)))
      (error "Slides are not the same length"))
    (setf (slide-lines slide1)
          (apply #'nconc
	         (mapcar* #'(lambda (line1 line2)
			      (list line1 line2))
		          (slide-lines slide1)
		          (slide-lines slide2))))
    (slide-insert slide1)
    ))

(defun append-slides ()
  "Replaces the the current and previous slides with a single slide containing the lines of the first slide, followed by lines of the second slide.  Leaves point in the slide after the newly-created slide."
  (interactive)
  (moveto-prev-slide)
  (let* ((slide1 (kill-slide-as-list))
	 (slide2 (kill-slide-as-list)))
    (setf (slide-lines slide1)
          (append (slide-lines slide1) (slide-lines slide2)))
    (setf (slide-comment slide1)
          (concat (slide-comment slide1) " " (slide-comment slide2)))
    (slide-insert slide1)))

(defun export-slides-to-qstit (bufname numfields fieldsize)
  (interactive "Bexport to buffer: \nnNumber of fields in each slide: \nnNumber of lines in each field: ")
  (save-excursion
    (let ((curbuf (current-buffer))
	  (export-buf (get-buffer-create bufname)))
      (if (equal export-buf (current-buffer)) (error "Cannot overwrite current buffer"))
      (goto-char (point-min))
      (catch 'exit
	(while t
	  (let ((slide (slide-get-slide-at-point)))
            (when (eq 'content (slide-type slide))
	      (set-buffer export-buf)
	      (insert (list-to-qstit numfields (pad-to-fieldsize (slide-lines slide) numfields fieldsize) (slide-comment slide)) "\n" )
	      (set-buffer curbuf)))
	  (if (not (moveto-next-slide)) (throw 'exit nil)))))))

(defun pad-to-fieldsize (lines numfields fieldsize)
  (append
   (make-list (* numfields (ceiling (/ (- fieldsize (/ (length lines) numfields)) 2.0))) "")
   lines
   (make-list (* numfields (floor (/ (- fieldsize (/ (length lines) numfields)) 2.0))) "")
   ))

(defun list-to-qstit (numfields lines leading)
  (concat (if (and leading (string-match "[[:print:]]" leading)) (format "<%s>" leading) "")
	  (cond ((= numfields 1)
		 (mapconcat #'format-qstit-text lines "\\"))
		(t
		 (list-to-qstit
		  1
		  (cl-loop for field upfrom 0 below numfields
			   nconc (cl-loop for linenum upfrom field below (length lines) by numfields
				          collect (nth linenum lines)))
		  nil
		  )))))

(defun format-qstit-text (line)
  "Takes one line of text content and formats it for output into a qstit file"
  (replace-regexp-in-string
   "  +"
   #'(lambda (match) (apply #'concat (make-list (length match) "&nbsp;")))
   line)
  ;; (identity line)
  )
    
; Moves the line before point to the end of the previous line
(defun subtxt-splice-to-prev-line ()
  (interactive)
  (subtxt-with-macro [?\C-  ?\C-a ?\C-x ?\C-k ?\C-p ?\C-p ?\C-e ?  ?\C-y ?\C-n ?\C-n ?\C-a]))

(defun subtxt-with-macro (macro)
  (let ((old-buffer-undo-list buffer-undo-list)
        new-buffer-undo-list)
    (let ((buffer-undo-list nil))
      (execute-kbd-macro macro)
      (setq new-buffer-undo-list buffer-undo-list))
    (setq buffer-undo-list
          (append (delete nil new-buffer-undo-list)
                  old-buffer-undo-list))))

; Moves the end of the line after point to a new line.
(defun subtxt-split-to-next-line ()
  (interactive)
  (subtxt-with-macro [?\C-k ?\C-n ?\C-a ?\C-n ?\C-y return]))

; Copies the speaker (portion before :) of the current line to the next line
(defun subtxt-copy-speaker-to-next-line ()
  (interactive)
  (subtxt-with-macro [?\C-a ?\C-  ?\C-s ?: ?\C-m ?\M-w ?\C-n ?\C-a ?\C-y ? ]))

; Suggest M-C-p
(defun promote-speaker ()
  (interactive)
  (subtxt-with-macro [?\C-a ?\C-  ?\C-e ?\C-x ?\C-k ?\C-d ?\C-y ?: ?  ?\C-n ?\C-a ?\C-y ?: ? ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minor mode
;;;

(defconst subtxt-font-lock-keywords
  '(
    ("^\\(--\\)\\([[:print:]]*\\)$" (1 'custom-comment) (2 'font-lock-comment-face))
    ("^\\(\\([[:upper:]\s-&,]\\)+\\):" 1 'font-lock-variable-name-face)
    ))
         
(define-minor-mode subtxt-mode
  "Minor mode for editing sub-txt files"
  :init-value nil
  :lighter " st"
  :keymap '(
            ([next] . next-slide)
            ("\M-a" . beginning-of-slide)
            ([prior] . prev-slide)
            ([f1] . merge-slides)
            ([f2] . append-slides)
            ([f3] . merge-names)
            ([f4] . undo)
            ([f6] . subtxt-splice-to-prev-line)
            ("\C-c\C-p" . subtxt-splice-to-prev-line)
            ([f7] . subtxt-split-to-next-line)
            ("\C-c\C-n" . subtxt-split-to-next-line)
            ([f8] . next-slide)
            ([f9] . prev-slide)
            ([f10] . merge-slides)
            ([f11] . append-slides)
            ([f12] . undo)
            )
  (set (make-local-variable 'font-lock-defaults) '(subtxt-font-lock-keywords t))
  (set (make-local-variable 'whitespace-style) '(face tabs lines-tail))
  (set (make-local-variable 'whitespace-line-column) 70)
  (whitespace-mode (if subtxt-mode 1 -1))
  )

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Web title player
;;;

(require 'websocket)
(require 'xmlgen)

(define-minor-mode subtxt-player-mode
  "Minor mode for playing sub-txt files"
  :init-value nil
  :lighter " play"
  :keymap '(
            ([next] . next-slide)
            ([prior] . prev-slide)
            ([pause] . sub-webplayer-toggle-suspend)
            ([end] . sub-webplayer-toggle-hide)
            )
  (if subtxt-player-mode
      (progn
        (sub-webplayer-start)
        (sub-preprocess-slides)
        (sub-webplayer-resume-slides))
    (progn
      (sub-webplayer-suspend-slides))))

(defvar sub-webplayer-server nil)
(defvar sub-webplayer-clients nil)

(defun sub-webplayer-start ()
  "Starts the websocket server for the web player to connect to"
  (interactive)
  (when (not sub-webplayer-server)
    (setq sub-webplayer-server
          (websocket-server
           6839
           :host 'local
           :on-open #'sub-webplayer-onopen
           :on-close #'sub-webplayer-onclose
           ))
    (sub-webplayer-resume-slides)))

(defun sub-webplayer-stop ()
  "Stops the webplayer's websocket server"
  (interactive)
  (when sub-webplayer-server
    (websocket-server-close sub-webplayer-server)
    (setq sub-webplayer-server nil)
    (setq sub-webplayer-clients nil)
    (sub-webplayer-suspend-slides)))

(defun sub-webplayer-toggle-suspend ()
  (interactive)
  (if (member 'sub-webplayer-display-current-slide slide-change-hook)
      (sub-webplayer-suspend-slides)
    (sub-webplayer-resume-slides)))

(defun sub-webplayer-resume-slides ()
  "automatically shows the current slide when navigating"
  (interactive)
  (add-hook 'slide-change-hook 'sub-webplayer-display-current-slide))
  
(defun sub-webplayer-suspend-slides ()
  "Turns off auto-display during navigation"
  (interactive)
  (remove-hook 'slide-change-hook 'sub-webplayer-display-current-slide))

(defun sub-webplayer-send (object)
  "broadcasts an object to all clients, formatted as json"
  (sub-webplayer-send-to-clients (json-encode object)))
   
(defun sub-webplayer-onopen (socket)
  "Registers a client for receiving broadcasts"
  (setq sub-webplayer-clients (cons socket sub-webplayer-clients))
  (sub-webplayer-refresh-slide))

(defun sub-webplayer-onclose (socket)
  "Removes a client from receiving broadcasts"
  (setq sub-webplayer-clients (remove socket sub-webplayer-clients)))

(defvar sub-webplayer-current-overlay nil)

(defun sub-move-webplayer-overlay (begin end)
  (unless sub-webplayer-current-overlay
    (setq sub-webplayer-current-overlay (make-overlay begin end))
    (overlay-put sub-webplayer-current-overlay 'face 'whitespace-trailing)
    )
  (move-overlay sub-webplayer-current-overlay begin end))

(defun sub-webplayer-display-current-slide ()
  "Displays the slide currently at point, in the connected web player"
  (interactive)
  (let ((slide (slide-get-slide-at-point)))
    (recenter-top-bottom 10)
    (sub-move-webplayer-overlay (slide-begin slide) (slide-end slide))
    (sub-webplayer-replace-slide slide)))

(defvar sub-webplayer-hidden nil)

(defun sub-webplayer-toggle-hide ()
  (interactive)
  (if sub-webplayer-hidden
      (sub-webplayer-show)
    (sub-webplayer-hide)))

(defun sub-webplayer-hide ()
  "Turns off the webplayer's slide display"
  (interactive)
  (setq sub-webplayer-hidden t)
  (sub-webplayer-send '((command . hide))))

(defun sub-webplayer-show ()
  "Turns on the webplayer's slide display"
  (interactive)
  (setq sub-webplayer-hidden nil)
  (sub-webplayer-send '((command . show))))

(defun sub-webplayer-set-style (style)
  (sub-webplayer-send `((command . style)
                        (style . ,style))))
  
(defvar sub-webplayer-current-slide nil)

(defun sub-webplayer-replace-slide (slide)
  (setq sub-webplayer-current-slide slide)
  (sub-webplayer-refresh-slide))

(defun sub-webplayer-refresh-slide ()
  (sub-webplayer-send-to-clients (sub-webplayer-format-slide sub-webplayer-current-slide)))

(defun sub-webplayer-send-to-clients (msg)
  (seq-do
   #'(lambda (client)
       (websocket-send-text client msg))
   sub-webplayer-clients))


(defun sub-webplayer-format-slide (slide)
  (json-encode `((command . newslide)
                 (comment . ,(slide-comment slide))
                 (content . ,(slide-html-format-slide slide)))))

(defun slide-html-format-slide (slide)
  "Applies the right slide template to the slide"
  (let ((formatter (or
                    (cdr (assoc (slide-template slide) subtxt-webplayer-templates))
                    "")))
    (funcall formatter slide)))

(defvar subtxt-slide-widths nil
  "A list of (LENGTH . POS), representing the LENGTH and POSition of each slide in the deck, sorted descending by LENGTH")

(defvar subtxt-webplayer-templates nil
  "A alist of (TNAME . TFUNC) associations, TNAME is the name of a template, an TFUNC is a function taking the argument SLIDE, a slide structure, and returning string of HTML which will display the slide")

(defun sub-preprocess-slides ()
  "Scan through all the slides in the current buffer, collecting info about them"
  (interactive)
  (setq subtxt-slide-widths nil)
  (setq subtxt-webplayer-templates nil)
  
  (let ((lengths '()))
    (sub-foreach-slide
     #'(lambda (pos slide)

         ;; Stash away the width of the slides
         (when (eq 'content (slide-type slide))
           (setq lengths (cons (cons (slide-width slide) pos) lengths)))

         ;; Process any layout templates
         (when (eq 'template (slide-type slide))
           (setq subtxt-webplayer-templates
                 (cons (cons (slide-template slide)
                             (slide-make-template-fn slide))
                       subtxt-webplayer-templates)))))
    
    (setq subtxt-slide-widths (sort lengths #'(lambda (a b) (< (car b) (car a)))))))

(defun slide-width (slide)
  "Returns the width of the longest line of a slide (in characters)"
  (cl-reduce #'max (mapcar #'length (slide-lines slide)) :initial-value 0))

(defun slide-make-template-fn (template-slide)
  (let ((template-lines
         (mapcar #'(lambda (line)
                     (car (read-from-string line)))
                 (slide-lines template-slide))))
    `(lambda (slide)
       (let ((lines (slide-lines slide)))
         (xmlgen
          (remove nil (list 'div ,@template-lines)))))))


(defun slide-default-formatter (slide)
  "Formats the lines into HTML, in the same order that they are given"
  (xmlgen `(div
            ,@(mapcar #'(lambda (line)
                          `(div :class "line" ,line))
                      (slide-lines slide)))))
  
(defun sub-foreach-slide (fn)
  "For each slide in the current buffer, call FN with the args:
POS: the position of the slide,
SLIDE: The slide struct"
  (save-excursion
    (let ((curbuf (current-buffer)))
      (goto-char (point-min))
      (catch 'exit
	(while t
	  (let ((lines (slide-get-slide-at-point)))
            (funcall fn (point) lines))
	  (if (not (moveto-next-slide)) (throw 'exit nil)))))))

(provide 'subtxt)
