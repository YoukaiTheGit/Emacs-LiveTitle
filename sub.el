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

(require 'cl)
(require 'whitespace)

(defun goto-slide-at-pos (p)
  (goto-char p))

(defun next-slide ()
  "Moves to beginning of next slide.  Returns NIL if no next"
  (interactive)
  (goto-char (point-at-eol))
  (if (search-forward "--" nil t)
      (goto-slide-at-pos (point-at-bol))
    nil))

(defun beginning-of-slide ()
  (interactive)
  (goto-char (point-at-eol))
  (search-backward "--")
  (goto-char (point-at-bol)))

(defun prev-slide ()
  (interactive)
  (goto-char (point-at-eol))
  (search-backward "--")
  (search-backward "--")
  (goto-slide-at-pos (point-at-bol)))

;; Our internal representation for a slide is a list of strings.
;; The first string is the "comment" for the slide.  In text form,
;; it is the content just after the after the '--' marker.
;; The remaining strings are the content lines of the slide, minus the EOL character.

(defmacro slide-comment (slide)
  `(car ,slide))

(defmacro slide-lines (slide)
  `(cdr ,slide))

(defun slide-contents-to-list (slide)
  (let* ((raw (split-string slide "\n" nil)) ; first elt is what trails slide separator
         (l (cons (substring (car raw) 2) (cdr raw))))
    (reverse (cdr (reverse l))))) ; last elt is empty string before next slide separator

(defun slide-as-list ()
  "Converts the current slide from the buffer to a list of lines"
  (save-excursion
    (beginning-of-slide)
    (let ((bos (point)))
      (next-slide)
      (let ((slide (buffer-substring-no-properties bos (point))))
	(slide-contents-to-list slide)))))

(defun kill-slide-as-list ()
  "Removes the current slide from the buffer, returning its lines as a list,
and leaving point at the beginning of the following slide"
  (beginning-of-slide)
  (let ((bos (point)))
    (next-slide)
    (let ((slide (buffer-substring-no-properties bos (point))))
      (kill-region bos (point))
      (slide-contents-to-list slide))))

(defun list-to-slide (lines)
  "Inserts a new slides whose lines are the elements of LINES"
  (insert "--")
  (mapc #'(lambda (line)
	    (insert line "\n"))
	lines))

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
    (when (/= (length slide1) (length slide2))
      (error "Slides are not the same length"))
    (list-to-slide 
     (mapcar* #'(lambda (line1 line2)
		  (format "%s%s%s" line1 delim line2))
	      slide1
	      slide2)))
  (prev-slide))

(defun interleave-slides ()
  "Replaces the the current and previous slides with a single slide containing the lines of both previous & current interleaved.  Leaves point in the slide after the newly-created slide."
  (interactive)
  (prev-slide)
  (let* ((slide1 (kill-slide-as-list))
	 (slide2 (kill-slide-as-list)))
    (when (/= (length slide1) (length slide2))
      (error "Slides are not the same length"))
    (list-to-slide
     (apply #'nconc
	    (mapcar* #'(lambda (line1 line2)
			 (list line1 line2))
		     slide1
		     slide2)))))

(defun append-slides ()
  "Replaces the the current and previous slides with a single slide containing the lines of the first slide, followed by lines of the second slide.  Leaves point in the slide after the newly-created slide."
  (interactive)
  (prev-slide)
  (let* ((slide1 (kill-slide-as-list))
	 (slide2 (kill-slide-as-list)))
    (list-to-slide (cons (concat (slide-comment slide1) " " (slide-comment slide2))
                         (nconc (slide-lines slide1) (slide-lines slide2))))))

(defun export-slides-to-qstit (bufname numfields fieldsize)
  (interactive "Bexport to buffer: \nnNumber of fields in each slide: \nnNumber of lines in each field: ")
  (save-excursion
    (let ((curbuf (current-buffer))
	  (export-buf (get-buffer-create bufname)))
      (if (equal export-buf (current-buffer)) (error "Cannot overwrite current buffer"))
      (goto-char (point-min))
      (catch 'exit
	(while t
	  (let ((lines (slide-as-list)))
	    (set-buffer export-buf)
	    (insert (list-to-qstit numfields (pad-to-fieldsize (slide-lines lines) numfields fieldsize) (slide-comment lines)) "\n" )
	    (set-buffer curbuf))
	  (if (not (next-slide)) (throw 'exit nil)))))))

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
		  (loop for field upfrom 0 below numfields
			nconc (loop for linenum upfrom field below (length lines) by numfields
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
    ("^\\(--\\)\\([[:print:]]*\\)" (1 highlight) (2 font-lock-comment-face))
    ("^\\([[:print:]]+?\\):" 1 font-lock-variable-name-face)
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
            ([f7] . subtxt-split-to-next-line)
            ([f8] . next-slide)
            ([f9] . prev-slide)
            ([f10] . merge-slides)
            ([f11] . append-slides)
            ([f12] . undo)
            )
  (set (make-local-variable 'font-lock-defaults) '(subtxt-font-lock-keywords t))
  (set (make-local-variable 'whitespace-style) '(face tabs lines-tail))
  (set (make-local-variable 'whitespace-line-column) 70)
  (whitespace-mode (if subtxt-mode 1 0))
  )

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Web title player
;;;

(require 'websocket)

(defvar sub-webplayer-server nil)
(defvar sub-webplayer-client nil)

(defun sub-webplayer-start ()
  "Starts the websocket server for the web player to connect to"
  (interactive)
  (if (not sub-webplayer-server)
      (setq sub-webplayer-server
            (websocket-server
             6839
             :host 'local
             :on-open #'sub-webplayer-onopen
             :on-close #'sub-webplayer-onclose
             ))))

(defun sub-webplayer-stop ()
  "Stops the webplayer's websocket server"
  (interactive)
  (when sub-webplayer-server
    (websocket-server-close sub-webplayer-server)
    (setq sub-webplayer-server nil)
    (setq sub-webplayer-client nil)))

(defun sub-webplayer-onopen (socket)
  (setq sub-webplayer-client socket)
  (sub-webplayer-refresh-slide))

(defun sub-webplayer-onclose (socket)
  (setq sub-webplayer-client nil))

(defun sub-webplayer-display-current-slide ()
  "Displays the slide currently at point, in the connected web player"
  (interactive)
  (let ((slide (slide-as-list)))
    (sub-webplayer-replace-slide slide)))

(defun sub-webplayer-hide ()
  "Turns off the webplayer's slide display"
  (interactive)
  (if sub-webplayer-client
      (websocket-send-text sub-webplayer-client (json-encode `((command . hide))))))

(defun sub-webplayer-show ()
  "Turns on the webplayer's slide display"
  (interactive)
  (if sub-webplayer-client
      (websocket-send-text sub-webplayer-client (json-encode `((command . show))))))

(defvar sub-webplayer-current-slide nil)

(defun sub-webplayer-replace-slide (slide)
  (setq sub-webplayer-current-slide slide)
  (sub-webplayer-refresh-slide))

(defun sub-webplayer-refresh-slide ()
  (if sub-webplayer-client
      (websocket-send-text sub-webplayer-client (sub-webplayer-format-slide sub-webplayer-current-slide))))


(defun sub-webplayer-format-slide (slide)
  (json-encode `((command . newslide)
                 (comment . ,(slide-comment slide))
                 (lines . ,(slide-lines slide)))))

   
