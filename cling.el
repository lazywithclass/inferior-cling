(require 'comint)

(defun cling (&optional flags)
  "Move to the buffer containing Cling, or create one if it does not exist. Defaults to C++11"
  (interactive)
  (let ((flags (or flags "-std=c++11")))
    (make-comint "inferior-cling" "cling" nil flags)
    (switch-to-buffer-other-window "*inferior-cling*"))

  ;; TODO this should go in an init function on its own
  (spacemacs/declare-prefix-for-mode 'c++-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode
    "'" 'cling
    "er" 'cling-send-region
    "eb" 'cling-send-buffer
    "ef" 'cling-wrap-defun-and-send
    "el" 'cling-send-line))

(defun cling-send-string (string &optional process)
  "Send a string terminated with a newline to the inferior-cling buffer.
   Has the effect of executing a command"
  (let ((process (or process (get-process "inferior-cling"))))
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun cling-send-region (start end)
  "Sends the region in the current buffer between `start` and `end` to
   the inferior-cling buffer. Sends the currently selected region when
   called interactively."
  (interactive "r")
  (cling-send-string (buffer-substring start end)))

(defun cling-send-buffer ()
  "Sends the current buffer to the inferior-cling buffer."
  (interactive)
  (cling-send-region (point-min) (point-max))) ;; do i want to wrap-raw this?

(defun cling-send-block ()
  (interactive)
  (let* ((p (point)))
    (mark-paragraph)
    (cling-send-region (region-beginning) (region-end))
    (goto-char p)))

(defun cling-send-line ()
  (interactive)
  (cling-send-block (line-beginning-position) (line-end-position)))

(defun cling-wrap-raw (string)
  "Wraps `string` in \".rawInput\", which tells Cling to accept
   function definitions"
  (format ".rawInput\n%s\n.rawInput" string))

(defun cling-wrap-region-and-send (start end)
  "Sends the region between start and end (currently selected when
   called interactively) to cling in raw input mode "
  (interactive "r")
  (cling-send-string (cling-wrap-raw (buffer-substring start end))))

(defun flatten-function-def ()
  "Flattens a function definition into a single line. This makes it
  easier to send to the inferior-cling buffer"
  (interactive)
  (replace-regexp "
" "" nil (mark) (point))) ;; Why did I do this again?

(defun select-defun ()
  "Selects the defun containing the point. Currently only works when
   point is on the line where the function's name is declared."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark (point))
  (re-search-forward "{")
  (save-excursion
    (flatten-function-def))
  (backward-char)
  (forward-sexp))

(defun cling-wrap-defun-and-send ()
  "Sends the current defun to cling in raw input mode. Currently
   only works when point is on the first line of function definition."
  (interactive)
  (save-excursion
    (select-defun)
    (cling-wrap-region-and-send (mark) (point))
    (undo)
    (undo))) ;; this is a rather leaky way of doing temporary changes. there should be some way to save buffer contents or something, probably uses with-temp-buffer

(defun cling-switch-to-repl ()
  (interactive)
  (if (get-buffer "*inferior-cling*")
      (switch-to-buffer-other-window "*inferior-cling*")
    (cling)))

(provide 'cling)
