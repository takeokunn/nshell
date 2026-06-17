;;; Compatibility load unit for insert-mode editing operations.
;;;
;;; The implementation is split by responsibility:
;;; - input-state-helpers.lisp: state copying, normalization, abbreviation helpers
;;; - input-state-buffer.lisp: primitive buffer edits and simple edit commands
;;; - input-state-words.lisp: token scanning and word motion
;;; - input-state-words.lisp: word case transforms and transpose
;;; - input-state-undo.lisp: undo/redo snapshots and transition recording
;;; - input-state-suggestion.lisp: autosuggestion acceptance
;;; - input-state-completion.lisp: completion cycling
;;; - input-state-kill-yank.lisp: kill-ring operations

(in-package #:nshell.presentation)
