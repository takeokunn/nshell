;;; Core data model for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defconstant +max-input-buffer-size+ 4096
  "Maximum editable input buffer length accepted by `reduce-input-state'.")

(deftype input-mode ()
  "Input reducer modes."
  '(member :insert :search))

(deftype output-event ()
  "Events emitted by `reduce-input-state' for an outer, effectful REPL loop."
  '(member :redraw :execute :complete :suggest-update :search-start :search-update
    :history-prev :history-next :insert-last-argument :clear-screen :none :quit))

(defstruct (input-state (:constructor make-input-state
                                      (&key (buffer "")
                                            (cursor-pos 0)
                                            (completion-index -1)
                                            (completion-base-buffer nil)
                                            (completion-base-cursor nil)
                                            (last-candidates nil)
                                            suggestion
                                            (mode :insert)
                                            (abbreviation-expander nil)
                                            (kill-ring nil)
                                            (last-yank-start nil)
                                            (last-yank-end nil)
                                            (last-yank-index nil)
                                            (last-argument-start nil)
                                            (last-argument-end nil)
                                            (last-argument-index nil)
                                            (search-query "")
                                            (search-original-buffer "")
                                            (search-original-cursor nil)
                                            (search-index 0)
                                            (undo-stack nil)
                                            (redo-stack nil))))
  "Pure line editor state.

BUFFER is the current editable text. CURSOR-POS is an index between 0 and
the buffer length. COMPLETION-INDEX and LAST-CANDIDATES model fish-style
completion cycling. COMPLETION-BASE-BUFFER keeps the buffer that produced
the current candidate list so cycling replaces the same token repeatedly.
COMPLETION-BASE-CURSOR keeps the cursor position that produced that list.
SUGGESTION is the gray autosuggestion tail, if any. MODE is either :INSERT
or :SEARCH. ABBREVIATION-EXPANDER is an optional function from the token
before cursor to its replacement string. KILL-RING stores killed text for
later yank operations. LAST-ARGUMENT-* stores the editable span created by the
last Alt-. history insertion so repeated Alt-. can replace it with older
arguments. SEARCH-QUERY, SEARCH-ORIGINAL-BUFFER, SEARCH-ORIGINAL-CURSOR, and
SEARCH-INDEX model an effect-free reverse history search; the outer REPL
supplies matching history rows. UNDO-STACK and REDO-STACK store editable line
snapshots for fish/readline-style local editing undo."
  (buffer "" :type string)
  (cursor-pos 0 :type integer)
  (completion-index -1 :type integer)
  (completion-base-buffer nil :type (or null string))
  (completion-base-cursor nil :type (or null integer))
  (last-candidates nil :type list)
  (suggestion nil :type (or null string))
  (mode :insert :type input-mode)
  (abbreviation-expander nil :type (or null function))
  (kill-ring nil :type list)
  (last-yank-start nil :type (or null integer))
  (last-yank-end nil :type (or null integer))
  (last-yank-index nil :type (or null integer))
  (last-argument-start nil :type (or null integer))
  (last-argument-end nil :type (or null integer))
  (last-argument-index nil :type (or null integer))
  (search-query "" :type string)
  (search-original-buffer "" :type string)
  (search-original-cursor nil :type (or null integer))
  (search-index 0 :type integer)
  (undo-stack nil :type list)
  (redo-stack nil :type list))
