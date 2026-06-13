;;; nshell package definitions
;;; DDD architecture: domain/ must not import from application/, infrastructure/, or presentation/

(eval-when (:compile-toplevel :load-toplevel :execute)
;; -- Main package ------------------------------------------
(defpackage #:nshell
  (:use #:cl)
  (:export #:main))

;; -- Domain packages (pure, no side effects) ----------------
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
  (:export #:make-signal #:signal-name #:signal-number #:signal-p #:signal=
           #:+sigint+ #:+sigterm+ #:+sigtstp+ #:+sigcont+ #:+sigchld+))

(defpackage #:nshell.domain.execution
  (:use #:cl)
  (:export #:make-command #:command-name #:command-args
            #:make-pipeline #:pipeline-p #:pipeline-commands
            #:make-pipeline-plan #:pipeline-plan-p #:pipeline-plan-stages
            #:pipeline-stage #:pipeline-stage-p #:pipeline-stage-command
            #:pipeline-stage-pipe-config #:pipe-config #:pipe-config-p
            #:pipe-config-stdin #:pipe-config-stdout #:pipe-config-index
            #:pipe-config-last-p #:pipeline-stage-count
            #:make-job #:job-id #:job-state #:job-pipeline
            #:job-state-valid-p #:job-state-transition #:command-to-list #:pipeline-length #:pipeline-empty-p #:pipeline-single-command-p #:job-running-p #:job-stopped-p #:job-completed-p #:job-pgid #:job-exit-code #:job-state-kw #:make-job-monitor #:monitor-find-job
            #:job-state-kw #:job-exit-code #:job-pids #:job-command-line #:job-background-p))

(defpackage #:nshell.domain.parsing
  (:use #:cl)
  (:export #:tokenize #:parse-command-line #:parse-result
           #:token-type #:token-value #:token-start #:token-end #:make-token
           #:ast-node-type #:make-command-node #:make-pipeline-node
           #:make-argument-node #:make-operator-node #:make-error-node
           #:command-node-p #:pipeline-node-p #:sequence-node-p
                #:command-node-command #:command-node-args
                #:sequence-node-commands #:pipeline-node-commands
                #:sequence-node-separators
                #:command-node-arg-values #:arg-value #:arg-quoted-p
           #:pipeline-node-commands
            #:var-p #:make-var #:unify #:walk #:extend-bindings #:backtrack #:unify-p
            #:parse-complete-p #:parse-errors #:parse-result-ast #:parse-result-incomplete))

(defpackage #:nshell.domain.environment
  (:use #:cl)
  (:export #:env-var #:env-var-p #:make-env-var
           #:env-var-name #:env-var-value #:env-var-exported-p
           #:environment #:environment-p #:make-environment
           #:environment-vars #:make-default-environment #:inject-os-environment
           #:env-get #:env-set #:env-unset #:env-export #:env-list))

(defpackage #:nshell.domain.expansion
  (:use #:cl)
  (:import-from #:nshell.domain.environment #:env-get)
  (:export #:*glob-directory-files-fn* #:*glob-subdirectories-fn*
           #:expand-variables #:expand-tilde #:expand-glob #:expand-all))

(defpackage #:nshell.domain.completion
  (:use #:cl)
  (:export #:make-candidate #:candidate-text #:candidate-kind
            #:candidate-description #:candidate-score
            #:make-knowledge-base #:kb-add-command #:kb-add-option #:kb-query
            #:make-fact #:make-rule #:fact-p #:rule-p
            #:assert-fact! #:assert-rule! #:prove #:prove-all #:rule-complete
            #:complete #:complete-command #:complete-argument))

(defpackage #:nshell.domain.history
  (:use #:cl)
  (:export #:make-history-entry #:entry-text #:entry-timestamp #:entry-exit-code
           #:make-history #:history-add #:history-search #:history-all
           #:history-merge #:history-dedup
           #:history-previous #:history-next #:history-reset-navigation))

(defpackage #:nshell.domain.job-control
  (:use #:cl)
  (:export #:make-job-monitor #:monitor-add-job #:monitor-update
            #:monitor-jobs #:monitor-find-job
            #:monitor-remove-job
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
           #:prompt-exit-code #:prompt-segment #:prompt-right-segments
           #:render-prompt-model #:render-right-prompt-model))

;; -- Application packages -----------------------------------
(defpackage #:nshell.application
  (:use #:cl)
  (:export #:*job-monitor* #:*shell-pgid* #:*foreground-job-pgid*
            #:make-event-dispatcher #:publish-event #:subscribe #:drain-events
            #:execute-command-line #:execute-pipeline-use-case #:execute-pipeline
            #:execute-pipeline-cps
            #:execute-external
            #:fg #:bg #:jobs #:disown #:interrupt-foreground #:suspend-foreground
            #:history-suggestion #:search-history-use-case))

;; -- Infrastructure packages --------------------------------
(defpackage #:nshell.infrastructure.acl
  (:use #:cl)
  (:export #:*exported-environment*
           #:spawn-command #:spawn-pipeline #:wait-job
            #:spawn-async
            #:kill-process #:os-signal->domain #:redirect-output #:redirect-input #:restore-redirects #:domain-signal->os
            #:install-signal-handlers
            #:open-pty #:with-pty #:pty-read #:pty-write #:pty-close #:make-pty-stream
            #:set-process-group #:set-foreground-pgroup #:get-foreground-pgroup
            #:make-process-group-leader #:reap-children #:get-terminal-size
            #:run-external #:spawn-pipeline))

(defpackage #:nshell.infrastructure.terminal
  (:use #:cl)
  (:export #:with-raw-terminal #:enable-raw-mode #:restore-terminal-mode
            #:ansi-clear-screen #:ansi-clear-line #:ansi-move-cursor
            #:ansi-set-color #:ansi-reset #:ansi-bold #:ansi-dim
            #:ansi-color-code
            #:make-screen #:screen-render #:screen-diff
            #:read-key-event
            #:key-event #:key-event-p #:make-key-event
            #:key-event-type #:key-event-char #:key-event-number))

(defpackage #:nshell.infrastructure.persistence
  (:use #:cl)
  (:export #:*history-file-path-override*
           #:load-history-file #:append-history-entry
           #:vacuum-history #:history-file-path
           #:load-config #:save-config #:config-file-path))

;; -- Presentation packages ----------------------------------
(defpackage #:nshell.presentation
  (:use #:cl)
  (:export #:input-state #:input-state-p #:make-input-state
            #:input-state-buffer #:input-state-cursor-pos
            #:input-state-completion-index #:input-state-last-candidates
            #:input-state-suggestion #:input-state-mode
            #:reduce-input-state #:key-event-type #:key-event-char
            #:key-event-number #:output-event
            #:run-repl #:trampoline #:done #:render-prompt #:render-input-line
            #:compute-suggestion #:accept-suggestion
            #:render-completions #:cycle-completion #:apply-completion
            #:highlight-line #:highlight-span #:highlight-role
           #:highlight->ansi #:theme-color->ansi #:segment-kind->role
           #:make-input-state #:input-state-buffer #:reduce-input-state
           #:input-state-cursor #:input-state-mode #:output-event))
)
