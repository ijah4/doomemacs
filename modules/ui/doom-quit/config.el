;;; ui/doom-quit/config.el -*- lexical-binding: t; -*-

(defvar +doom-quit-messages
  `(;; from Doom 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    ,(format "I wouldn't leave if I were you. %s is much worse."
             (if (featurep :system 'windows) "DOS" "UNIX"))
    "Don't leave yet -- There's a demon around that corner!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; from Portal
    "Thank you for participating in this Aperture Science computer-aided enrichment activity."
    "You can't fire me, I quit!"
    "I don't know what you think you are doing, but I don't like it. I want you to stop."
    "This isn't brave. It's murder. What did I ever do to you?"
    "I'm the man who's going to burn your house down! With the lemons!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    ;; Custom
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "It's not like I'll miss you or anything, b-baka!"
    "Wake up, Mr. Stallman. Wake up and smell the ashes."
    "You are *not* prepared!"
    "Please don't go. The drones need you. They look up to you.")
  "A list of quit messages, picked randomly by `+doom-quit'. Taken from
http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defvar doom--restart-in-progress nil
  "Used to prevent infinite recursion.
This is non-nil if `doom--advice-kill-emacs-dispatch' has called
`restart-emacs'.")

(defvar doom--restart-emacs-eager-hook-functions
  ;; This list contains hooks that I determined via profiling to be
  ;; slow (double-digit milliseconds).
  '(prescient--save
    recentf-save-list
    savehist-autosave
    org-persist-gc
    save-place-kill-emacs-hook)
  "List of functions on `kill-emacs-hook' which can be run eagerly.
If actually present on `kill-emacs-hook', then these functions
are run immediately on `save-buffers-kill-emacs'. This means that
Emacs shutdown appears to be slightly faster.

Functions can only be added here if it is okay to run them even
when shutting down Emacs is canceled. However, it is fine to put
functions here that aren't actually present on `kill-emacs-hook'.")

(defvar doom--restart-emacs-eager-hook-functions-run nil
  "List of functions on `kill-emacs-hook' which have been run eagerly.
The global value of this variable is irrelevant; it is always
bound dynamically before being used.")

(defadvice! doom--advice-kill-emacs-dispatch
  (save-buffers-kill-emacs &optional arg)
  "Allow restarting Emacs or starting a new session on shutdown."
  :around #'save-buffers-kill-emacs
  (if doom--restart-in-progress
      (funcall save-buffers-kill-emacs arg)
    (let ((doom--restart-in-progress t)
          (confirm-kill-emacs nil)
          ;; Don't mutate the global value.
          (doom--restart-emacs-eager-hook-functions-run nil)
          (prompt (format "%s  %s"
                          (propertize
                           (nth (random (length +doom-quit-messages))
                                +doom-quit-messages)
                           'face '(italic default))
                          (propertize "Really quit?(y/n/r/e/k)"
                                      'face 'minibuffer-prompt)))
          (key nil))
      (dolist (func doom--restart-emacs-eager-hook-functions)
        ;; Run eager hook functions asynchronously while waiting for
        ;; user input. Use a separate idle timer for each function
        ;; because the order shouldn't be important, and because
        ;; that way if we don't actually restart then we can cancel
        ;; out faster (we don't have to wait for all the eager hook
        ;; functions to run).
        (run-with-idle-timer
         0 nil
         (lambda ()
           (when (and doom--restart-in-progress (memq func kill-emacs-hook))
             (letf! ((standard-output (lambda (&rest _)))
                     (defun message (&rest _))
                     (defun write-region
                         (start end filename &optional append visit lockname mustbenew)
                       (unless visit (setq visit 'no-message))
                       (funcall write-region start end filename append visit lockname mustbenew)))
               (funcall func))
             ;; Thank goodness Elisp is single-threaded.
             (push func doom--restart-emacs-eager-hook-functions-run)))))
      (while (null key)
        (let ((cursor-in-echo-area t))
          (when minibuffer-auto-raise
            (raise-frame (window-frame (minibuffer-window))))
          (setq key (read-key prompt))
          ;; No need to re-run the hooks that we already ran
          ;; eagerly. (This is the whole point of those
          ;; shenanigans.)
          (let ((kill-emacs-hook
                 (cl-remove-if
                  (lambda (func)
                    (memq
                     func
                     doom--restart-emacs-eager-hook-functions-run))
                  kill-emacs-hook)))
            (pcase key
              ((or ?y ?Y) (funcall save-buffers-kill-emacs arg))
              ((or ?n ?N))
              ((or ?r ?R) (require 'restart-emacs) (restart-emacs arg))
              ((or ?e ?E) (restart-emacs-start-new-emacs arg))
              ((or ?k ?K) (let (kill-emacs-hook) (kill-emacs)))
              (?\C-g (signal 'quit nil))
              (_ (setq key nil))))))
      (message "%s%c" prompt key))))
