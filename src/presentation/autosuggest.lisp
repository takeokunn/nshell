(in-package #:nshell.presentation)

(defun autosuggest-token-prefix (input)
  (let ((context (nshell.domain.completion:completion-context-for input)))
    (if (nshell.domain.completion:completion-context-command-position-p context)
        (nshell.domain.completion:completion-context-command context)
        (nshell.domain.completion:completion-context-argument-prefix context))))

(defun completion-suggestion (knowledge-base input &key path)
  (when (and knowledge-base
             (not (nshell.domain.parsing:shell-input-blank-p input)))
    (handler-case
        (let* ((prefix (autosuggest-token-prefix input))
               (candidate
                 (first (nshell.domain.completion:complete knowledge-base
                                                           input
                                                           :path path))))
          (when candidate
            (let ((text (nshell.domain.completion:candidate-text candidate)))
              (when (and (<= (length prefix) (length text))
                         (string-equal prefix text :end2 (length prefix))
                         (< (length prefix) (length text)))
                (subseq text (length prefix))))))
      (error () nil))))

(defun compute-suggestion (history input &key knowledge-base path)
  (unless (nshell.domain.parsing:shell-input-blank-p input)
    (or (nshell.application:history-suggestion history input)
        (completion-suggestion knowledge-base input :path path))))

(defun accept-suggestion (input suggestion)
  (concatenate 'string input suggestion))
