;;; claude-code-ide-session.el --- Session struct for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Defines the `claude-code-ide-session' struct and the global session
;; table.  Extracted into its own file so that byte-compilation of
;; dependent files (e.g. `claude-code-ide-mcp') has access to the
;; struct's `setf' expanders without circular `require' chains.

;;; Code:

(require 'cl-lib)

(cl-defstruct claude-code-ide-session
  "Unified structure holding all state for a single Claude Code session.
Replaces the former separate `claude-code-ide-mcp-session' struct
and the directory-keyed process/session-id hash tables."
  session-id        ; unique identifier, e.g. "claude-project-20260223-143000"
  name              ; user-facing display name (e.g. "design"), nil for default
  directory         ; expanded project root path
  ;; Process & buffer
  process           ; terminal process
  buffer            ; terminal buffer
  ;; MCP state
  port              ; WebSocket server port
  server            ; WebSocket server object
  client            ; connected WebSocket client
  ping-timer        ; keepalive timer
  selection-timer   ; selection change debounce timer
  last-selection    ; last selection state for change detection
  last-buffer       ; last active buffer for change detection
  deferred          ; hash-table of deferred responses
  active-diffs      ; hash-table of active ediff sessions
  ;; Status tracking
  (status 'idle)      ; symbol: idle or working
  last-message        ; string: last assistant message or tool name
  status-updated-at   ; float-time: timestamp of last status update
  (stopped nil)       ; boolean: whether the agent has stopped
  (pending-permissions 0) ; integer: number of pending permission requests
  original-tab)     ; tab-bar tab where session was started

(defvar claude-code-ide--sessions (make-hash-table :test 'equal)
  "Hash table mapping session-id to `claude-code-ide-session' structs.
This is the single source of truth for all active sessions.")

(provide 'claude-code-ide-session)

;;; claude-code-ide-session.el ends here
