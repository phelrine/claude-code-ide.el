;;; claude-code-ide-dashboard.el --- Session dashboard for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Provides a tabulated-list-mode dashboard for viewing and managing
;; all active Claude Code sessions.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'claude-code-ide-session)
(require 'claude-code-ide)

(defvar claude-code-ide-dashboard-buffer-name "*claude-code-dashboard*"
  "Name of the session dashboard buffer.")

(defvar claude-code-ide-dashboard-message-max-length 60
  "Maximum length for the last activity message in the dashboard.")

(defun claude-code-ide-dashboard--format-status (status)
  "Format STATUS symbol for display with colored indicator."
  (let ((s (or status 'idle)))
    (concat (claude-code-ide--status-indicator s) " " (symbol-name s))))

(defun claude-code-ide-dashboard--truncate-message (msg)
  "Truncate MSG to a single line within `claude-code-ide-dashboard-message-max-length'."
  (let* ((oneline (replace-regexp-in-string "[\n\r]+" " " (or msg "")))
         (max claude-code-ide-dashboard-message-max-length))
    (if (<= (length oneline) max)
        oneline
      (concat (substring oneline 0 (- max 1)) "…"))))

(defun claude-code-ide-dashboard--entries ()
  "Generate tabulated-list entries from active sessions."
  (claude-code-ide--cleanup-dead-sessions)
  (let ((entries '()))
    (maphash
     (lambda (id session)
       (let* ((name (or (claude-code-ide-session-name session)
                        (claude-code-ide-session-session-id session)))
              (dir (abbreviate-file-name
                    (claude-code-ide-session-directory session)))
              (status (claude-code-ide-dashboard--format-status
                       (claude-code-ide-session-status session)))
              (msg (claude-code-ide-dashboard--truncate-message
                    (claude-code-ide-session-last-message session))))
         (push (list id (vector name dir status msg)) entries)))
     claude-code-ide--sessions)
    (nreverse entries)))

(defun claude-code-ide-dashboard-open-session ()
  "Open the session at point in a side window."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (session (gethash id claude-code-ide--sessions))
              (buffer (claude-code-ide-session-buffer session)))
    (if (buffer-live-p buffer)
        (claude-code-ide--display-buffer-in-side-window buffer)
      (user-error "Buffer for session no longer exists"))))

(defun claude-code-ide-dashboard-stop-session ()
  "Stop the session at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (session (gethash id claude-code-ide--sessions))
              (buffer (claude-code-ide-session-buffer session)))
    (when (yes-or-no-p (format "Stop session %s? "
                               (or (claude-code-ide-session-name session) id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (claude-code-ide-stop)))
      (revert-buffer t t))))

(defun claude-code-ide-dashboard-rename-session ()
  "Rename the session at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (session (gethash id claude-code-ide--sessions)))
    (let ((new-name (read-string "New session name: "
                                 (claude-code-ide-session-name session))))
      (setf (claude-code-ide-session-name session) new-name)
      (when-let ((buf (claude-code-ide-session-buffer session)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (rename-buffer
             (claude-code-ide--buffer-name-for-session session) t))))
      (revert-buffer t t))))

(defvar claude-code-ide-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-code-ide-dashboard-open-session)
    (define-key map (kbd "s")   #'claude-code-ide-dashboard-stop-session)
    (define-key map (kbd "r")   #'claude-code-ide-dashboard-rename-session)
    map)
  "Keymap for `claude-code-ide-dashboard-mode'.")

(define-derived-mode claude-code-ide-dashboard-mode tabulated-list-mode
  "Claude Sessions"
  "Major mode for viewing Claude Code sessions."
  (setq tabulated-list-format [("Name" 15 t)
                               ("Directory" 30 t)
                               ("Status" 20 t)
                               ("Last Activity" 0 t)])
  (setq tabulated-list-entries #'claude-code-ide-dashboard--entries)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun claude-code-ide-dashboard ()
  "Open the Claude Code session dashboard."
  (interactive)
  (let ((buf (get-buffer-create claude-code-ide-dashboard-buffer-name)))
    (with-current-buffer buf
      (claude-code-ide-dashboard-mode)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'claude-code-ide-dashboard)

;;; claude-code-ide-dashboard.el ends here
