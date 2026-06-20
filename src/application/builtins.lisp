(in-package #:nshell.application)

(defun %builtin-command-path (context args command)
  (let ((spec (%command-path-spec command)))
    (if args
        (let ((exit-code 0))
          (values
           (with-output-to-string (out)
             (dolist (name args)
               (multiple-value-bind (kind text)
                   (%describe-command-path
                    context name
                    (lambda (missing-name)
                      (%format-command-path-missing spec missing-name)))
                 (case kind
                   (:builtin
                    (format out (getf spec :builtin-format) name))
                   (:path
                    (format out (getf spec :path-format) name text))
                   (otherwise
                    (setf exit-code 1)
                    (write-string text out))))))
           exit-code))
        (%builtin-usage command (getf spec :usage)))))

(defun %builtin-type (context args)
  (let ((spec nshell.domain.completion:+type-builtin-spec+))
    (multiple-value-bind (options names error error-code)
        (%parse-type-options args)
      (cond
        (error
         (values error error-code))
        ((%type-options-help-p options)
         (%type-usage 0))
        ((null names)
         (%type-usage))
        (t
         (let ((exit-code 1)
               (mode (cond
                       ((%type-options-query-p options) :query)
                       ((%type-options-path-p options) :path)
                       ((%type-options-force-path-p options) :force-path)
                       ((%type-options-type-p options) :type)
                       (t :default))))
           (labels ((emit-candidates (name candidates out)
                      (setf exit-code 0)
                      (dolist (candidate (if (%type-options-all-p options)
                                             candidates
                                             (list (first candidates))))
                        (case mode
                          (:type
                           (format out "~a~%"
                                   (%type-kind-label (first candidate))))
                          (:path
                           (case (first candidate)
                             (:builtin
                              (format out (getf spec :path-builtin-format) name))
                             (:path
                              (format out (getf spec :path-only-format)
                                      (second candidate)))))
                          (:force-path
                           (when (eq (first candidate) :path)
                             (format out (getf spec :path-only-format)
                                     (second candidate))))
                          (otherwise
                           (%write-type-candidate out spec name candidate options))))))
             (if (eq mode :query)
                 (progn
                   (dolist (name names)
                     (when (%type-command-candidates context name options)
                       (setf exit-code 0)))
                   (values nil exit-code))
                 (let ((output
                         (with-output-to-string (out)
                           (dolist (name names)
                             (let ((candidates (%type-command-candidates context name options)))
                               (cond
                                 (candidates
                                  (emit-candidates name candidates out))
                                 ((eq mode :default)
                                  (write-string (%format-command-type-missing spec name) out))))))))
                  (values output exit-code))))))))))

(defun %builtin-which (context args)
  (%builtin-command-path context args "which"))

(defun expand-command-alias-node (command-node alias-table)
  (if (nshell.domain.parsing:command-node-p command-node)
      (let* ((command (nshell.domain.parsing:command-node-command command-node))
             (alias (gethash command alias-table)))
        (if alias
            (nshell.domain.parsing:with-complete-command-line (result alias-node alias)
              (if (nshell.domain.parsing:command-node-p alias-node)
                  (nshell.domain.parsing:make-command-node
                   (nshell.domain.parsing:command-node-command alias-node)
                   (append (nshell.domain.parsing:command-node-args alias-node)
                           (nshell.domain.parsing:command-node-args command-node)))
                  command-node))
            command-node))
      command-node))

(defun %install-builtin-registry ()
  (clrhash *builtin-registry*)
  (dolist (entry +builtin-registry-specs+)
    (setf (gethash (car entry) *builtin-registry*)
          (symbol-function (cdr entry)))))

(eval-when (:load-toplevel :execute)
  (%install-builtin-registry))
