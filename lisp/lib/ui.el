;;; lisp/lib/ui.el -*- lexical-binding: t; -*-

;;
;;; Public library

;;;###autoload
(defun doom-resize-window (window new-size &optional horizontal force-p)
  "Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise.
If FORCE-P is omitted when `window-size-fixed' is non-nil, resizing will fail."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
                      horizontal))))

;;;###autoload
(defun doom-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.

Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (or (not (ignore-errors (doom-real-buffer-list)))
      (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
      (ignore (message "Aborted"))))


;;
;;; Advice

;;;###autoload
(defun doom-recenter-a (&rest _)
  "Generic advice for recentering window (typically :after other functions)."
  (recenter))

;;;###autoload
(defun doom-preserve-window-position-a (fn &rest args)
  "Generic advice for preserving cursor position on screen after scrolling."
  (let ((row (cdr (posn-col-row (posn-at-point)))))
    (prog1 (apply fn args)
      (save-excursion
        (let ((target-row (- (line-number-at-pos) row)))
          (unless (< target-row 0)
            (evil-scroll-line-to-top target-row)))))))

;;;###autoload
(defun doom-shut-up-a (fn &rest args)
  "Generic advisor for silencing noisy functions.

In interactive Emacs, this just inhibits messages from appearing in the
minibuffer. They are still logged to *Messages*.

In tty Emacs, messages are suppressed completely."
  (quiet! (apply fn args)))


;;
;;; Hooks

;;;###autoload
(defun doom-apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))

;;;###autoload
(defun doom-disable-show-paren-mode-h ()
  "Turn off `show-paren-mode' buffer-locally."
  (setq-local show-paren-mode nil))

;;;###autoload
(defun doom-enable-line-numbers-h ()
  (display-line-numbers-mode +1))

;;;###autoload
(defun doom-disable-line-numbers-h ()
  (display-line-numbers-mode -1))


;;
;;; Commands

;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.

See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))

;;;###autoload
(defun doom/delete-frame-with-prompt ()
  "Delete the current frame, but ask for confirmation if it isn't empty."
  (interactive)
  (if (cdr (frame-list))
      (when (doom-quit-p "Close frame?")
        (delete-frame))
    (save-buffers-kill-emacs)))


(defun doom--enlargened-forget-last-wconf-h ()
  (set-frame-parameter nil 'doom--maximize-last-wconf nil)
  (set-frame-parameter nil 'doom--enlargen-last-wconf nil)
  (remove-hook 'doom-switch-window-hook #'doom--enlargened-forget-last-wconf-h))

;;;###autoload
(defun doom/window-maximize-buffer (&optional arg)
  "Close other windows to focus on this one.
Use `winner-undo' to undo this. Alternatively, use `doom/window-enlargen'."
  (interactive "P")
  (when (and (bound-and-true-p +popup-mode)
             (+popup-window-p))
    (+popup/raise (selected-window)))
  (delete-other-windows))

;;;###autoload
(defun doom/window-enlargen (&optional arg)
  "Enlargen the current window (i.e. shrinks others) so you can focus on it.
Use `winner-undo' to undo this. Alternatively, use
`doom/window-maximize-buffer'."
  (interactive "P")
  (let* ((window (selected-window))
         (dedicated-p (window-dedicated-p window))
         (preserved-p (window-parameter window 'window-preserved-size))
         (ignore-window-parameters t)
         (window-resize-pixelwise nil)
         (frame-resize-pixelwise nil))
    (unwind-protect
        (progn
          (when dedicated-p
            (set-window-dedicated-p window nil))
          (when preserved-p
            (set-window-parameter window 'window-preserved-size nil))
          (maximize-window window))
      (set-window-dedicated-p window dedicated-p)
      (when preserved-p
        (set-window-parameter window 'window-preserved-size preserved-p)))))

;;;###autoload
(defun doom/window-maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-left)) (delete-window))
    (while (ignore-errors (windmove-right)) (delete-window))))

;;;###autoload
(defun doom/window-maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-up)) (delete-window))
    (while (ignore-errors (windmove-down)) (delete-window))))

;;;###autoload
(defun doom/set-frame-opacity (opacity)
  "Interactively change the current frame's opacity.

OPACITY is an integer between 0 to 100, inclusive."
  (interactive '(interactive))
  (let* ((parameter
          (if (eq window-system 'pgtk)
              'alpha-background
            'alpha))
         (opacity
          (if (eq opacity 'interactive)
              (read-number "Opacity (0-100): "
                           (or (frame-parameter nil parameter)
                               100))
            opacity)))
    (set-frame-parameter nil parameter opacity)))

(defvar doom--narrowed-base-buffer nil)
;;;###autoload
(defun doom/narrow-buffer-indirectly (beg end)
  "Restrict editing in this buffer to the current region, indirectly.

This recursively creates indirect clones of the current buffer so that the
narrowing doesn't affect other windows displaying the same buffer. Call
`doom/widen-indirectly-narrowed-buffer' to undo it (incrementally).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive (if (region-active-p)
                   (list (doom-region-beginning) (doom-region-end))
                 (list (bol) (eol))))
  (deactivate-mark)
  (let ((orig-buffer (current-buffer)))
    (with-current-buffer (switch-to-buffer (clone-indirect-buffer nil nil))
      (narrow-to-region beg end)
      (setq-local doom--narrowed-base-buffer orig-buffer))))

;;;###autoload
(defun doom/widen-indirectly-narrowed-buffer (&optional arg)
  "Widens narrowed buffers.

This command will incrementally kill indirect buffers (under the assumption they
were created by `doom/narrow-buffer-indirectly') and switch to their base
buffer.

If ARG, then kill all indirect buffers, return the base buffer and widen it.

If the current buffer is not an indirect buffer, it is `widen'ed."
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "Buffer isn't narrowed"))
  (let ((orig-buffer (current-buffer))
        (base-buffer doom--narrowed-base-buffer))
    (cond ((or (not base-buffer)
               (not (buffer-live-p base-buffer)))
           (widen))
          (arg
           (let ((buffer orig-buffer)
                 (buffers-to-kill (list orig-buffer)))
             (while (setq buffer (buffer-local-value 'doom--narrowed-base-buffer buffer))
               (push buffer buffers-to-kill))
             (switch-to-buffer (buffer-base-buffer))
             (mapc #'kill-buffer (remove (current-buffer) buffers-to-kill))))
          ((switch-to-buffer base-buffer)
           (kill-buffer orig-buffer)))))

;;;###autoload
(defun doom/toggle-narrow-buffer (beg end)
  "Narrow the buffer to BEG END. If narrowed, widen it."
  (interactive (if (region-active-p)
                   (list (doom-region-beginning) (doom-region-end))
                 (list (bol) (eol))))
  (if (buffer-narrowed-p)
      (widen)
    (narrow-to-region beg end)))

;;
;;; windows layout

;;;###autoload
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

;;;###autoload
(defun doom/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;;;###autoload
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is
on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

;;;###autoload
(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is
on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

;;;###autoload
(defun doom/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'doom/split-window)
      (progn
        (jump-to-register :doom/split-window)
        (setq this-command 'doom/unsplit-window))
    (window-configuration-to-register :doom/split-window)
    (switch-to-buffer-other-window nil)))

;;;###autoload
(defun doom/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

;;
;;; pretty formfeed

;;;###autoload
(defun jah-insert-formfeed ()
  "Insert a form feed char (codepoint 12)"
  (interactive)
  (insert "\u000c\n"))

;;;###autoload
(defun jah-show-formfeed-as-line (&optional frame)
  "Display the formfeed ^L char as line."
  (interactive)
  (letf! (defun pretty-formfeed-line (window)
           (with-current-buffer (window-buffer window)
             (with-selected-window window
               (when (not buffer-display-table)
                 (setq buffer-display-table (make-display-table)))
               (aset buffer-display-table ?\^L
                     (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
               (redraw-frame))))
    (unless (minibufferp)
      (mapc 'pretty-formfeed-line (window-list frame 'no-minibuffer)))))

;;;###autoload
(dolist (hook '(window-configuration-change-hook
                window-size-change-functions
                after-setting-font-hook
                display-line-numbers-mode-hook))
  (add-hook hook #'jah-show-formfeed-as-line))

(provide 'doom-lib '(ui))
;;; ui.el ends here