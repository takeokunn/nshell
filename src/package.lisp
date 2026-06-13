;;; nshell package definitions
;;; DDD architecture: domain/ must not import from application/, infrastructure/, or presentation/

(eval-when (:compile-toplevel :load-toplevel :execute)
;; ── Main package ──────────────────────────────────────────
(defpackage #:nshell
  (:use #:cl)
  (:export #:main))

;; ── Domain packages (pure, no side effects) ────────────────
(defpackage #:nshell.domain.events
  (:use #:cl)
  (:export #:domain-event #:domain-event-p #:domain-event-type #:domain-event-timestamp
           #:make-domain-event #:event-type #:event-timestamp
           #:make-event #:event-type-p
           #:make-command-entered-event #:make-command-parsed-event
           #:make-parse-failed-event #:make-pipeline-started-event
           #:make-process-created-event #:make-process-exited-event
           #:make-pipeline-completed-event #:make-job-created-event
           #:make-job-stopped-event #:make-job-continued-event
           #:make-job-completed-event #:make-signal-caught-event
           #:make-command-appended-to-history-event #:make-completion-triggered-event))

(defpackage #:nshell.domain.signals
  (:use #:cl)
  (:export #:make-signal #:signal-name #:signal-number #:signal=
           #:+sigint+ #:+sigterm+ #:+sigtstp+ #:+sigcont+ #:+sigchld+))

(defpackage #:nshell.domain.execution
  (:use #:cl)
  (:export #:make-command #:command-name #:command-args
           #:make-pipeline #:pipeline-commands
           #:make-job #:job-id #:job-state #:job-pipeline
           #:job-state-valid-p #:job-state-transition
           #:job-state-kw #:job-exit-code))

(defpackage #:nshell.domain.parsing
  (:use #:cl)
  (:export #:tokenize #:parse-command-line #:parse-result
           #:token-type #:token-value #:token-start #:token-end #:make-token
           #:ast-node-type #:make-command-node #:make-pipeline-node
           #:make-argument-node #:make-operator-node #:make-error-node
           #:command-node-p #:pipeline-node-p #:command-node-command #:command-node-args
           #:pipeline-node-commands
           #:var-p #:make-var #:unify #:walk #:extend-bindings #:backtrack
           #:parse-complete-p #:parse-errors #:parse-result-ast #:parse-result-incomplete))

(defpackage #:nshell.domain.completion
  (:use #:cl)
  (:export #:make-candidate #:candidate-text #:candidate-kind
           #:candidate-description #:candidate-score
           #:make-knowledge-base #:kb-add-command #:kb-add-option #:kb-query
           #:complete #:complete-command #:complete-argument))

(defpackage #:nshell.domain.history
  (:use #:cl)
  (:export #:make-history-entry #:entry-text #:entry-timestamp #:entry-exit-code
           #:make-history #:history-add #:history-search #:history-all
           #:history-merge #:history-dedup))

(defpackage #:nshell.domain.job-control
  (:use #:cl)
  (:export #:make-job-monitor #:monitor-add-job #:monitor-update
           #:monitor-jobs #:monitor-find-job
           #:suspend-job #:resume-job #:foreground-job))

(defpackage #:nshell.domain.configuration
  (:use #:cl)
  (:export #:make-theme #:theme-color #:theme-name #:theme-set-color
           #:theme-p #:config-p
           #:make-config #:config-theme #:config-prompt
           #:default-theme #:default-config))

(defpackage #:nshell.domain.prompting
  (:use #:cl)
  (:export #:make-prompt-model #:prompt-hostname #:prompt-cwd
           #:prompt-exit-code #:prompt-segment
           #:render-prompt-model))

;; ── Application packages ───────────────────────────────────
(defpackage #:nshell.application
  (:use #:cl)
  (:export #:make-event-dispatcher #:publish-event #:subscribe #:drain-events
           #:execute-command-line #:execute-pipeline-use-case
           #:fg #:bg #:jobs #:interrupt-foreground #:suspend-foreground
           #:history-suggestion #:search-history-use-case))

;; ── Infrastructure packages ────────────────────────────────
(defpackage #:nshell.infrastructure.acl
  (:use #:cl)
  (:export #:spawn-command #:spawn-pipeline #:wait-job
           #:kill-process #:os-signal->domain #:domain-signal->os
           #:install-signal-handlers
           #:open-pty #:with-pty #:pty-read #:pty-write))

(defpackage #:nshell.infrastructure.terminal
  (:use #:cl)
  (:export #:with-raw-terminal #:enable-raw-mode
           #:ansi-clear-screen #:ansi-clear-line #:ansi-move-cursor
           #:ansi-set-color #:ansi-reset #:ansi-bold #:ansi-dim
           #:make-screen #:screen-render #:screen-diff
           #:read-key-event #:key-event-type #:key-event-char))

(defpackage #:nshell.infrastructure.persistence
  (:use #:cl)
  (:export #:load-history-file #:append-history-entry
           #:vacuum-history #:history-file-path
           #:load-config #:save-config #:config-file-path))

;; ── Presentation packages ──────────────────────────────────
(defpackage #:nshell.presentation
  (:use #:cl)
  (:export #:run-repl #:render-prompt #:render-input-line
           #:compute-suggestion #:accept-suggestion
           #:render-completions #:cycle-completion #:apply-completion
           #:highlight-line #:highlight-span #:highlight-role
           #:highlight->ansi))
)
