(in-package #:nshell.domain.parsing)

(defstruct (%token-reduction-state
            (:constructor %make-token-reduction-state))
  (all-cmds '() :type list)
  (current-args '() :type list)
  (current-cmd nil)
  (current-cmd-token nil)
  (pending-redirect-token nil)
  (pending-sep nil)
  (pending-sep-token nil)
  (errors '() :type list))

(defun %record-missing-redirect-target (state)
  (let ((pending-redirect-token (%token-reduction-state-pending-redirect-token state)))
    (when pending-redirect-token
      (push (%token-diagnostic
             :missing-redirection-target
             (format nil "Expected target after '~a'"
                     (token-value pending-redirect-token))
             pending-redirect-token)
            (%token-reduction-state-errors state))
      (setf (%token-reduction-state-pending-redirect-token state) nil))))

(defun %flush-token-reduction-command (state)
  (when (%token-reduction-state-current-cmd state)
    (%record-missing-redirect-target state)
    (push (list (make-command-node
                 (%token-reduction-state-current-cmd state)
                 (nreverse (%token-reduction-state-current-args state))
                 (when (%token-reduction-state-current-cmd-token state)
                   (list (token-start (%token-reduction-state-current-cmd-token state))
                         (token-end (%token-reduction-state-current-cmd-token state)))))
                (%token-reduction-state-pending-sep state)
                (%token-reduction-state-pending-sep-token state))
          (%token-reduction-state-all-cmds state))
    (setf (%token-reduction-state-current-cmd state) nil
          (%token-reduction-state-current-cmd-token state) nil
          (%token-reduction-state-current-args state) '()
          (%token-reduction-state-pending-redirect-token state) nil
          (%token-reduction-state-pending-sep state) nil
          (%token-reduction-state-pending-sep-token state) nil)))

(defun %record-token-reduction-separator (state separator token)
  (if (%token-reduction-state-current-cmd state)
      (progn
        (%record-missing-redirect-target state)
        (setf (%token-reduction-state-pending-sep state) separator
              (%token-reduction-state-pending-sep-token state) token)
        (%flush-token-reduction-command state))
      (push (%token-diagnostic
             :missing-command
             (format nil "Expected command before '~a'"
                     (%separator-text separator))
             token)
            (%token-reduction-state-errors state))))

(defun %reduce-token-stream (tokens)
  (let ((state (%make-token-reduction-state)))
    (dolist (tok tokens)
      (case (token-type tok)
        (:word
         (if (%token-reduction-state-current-cmd state)
             (progn
               (let ((style (token-quote-style tok)))
                 (push (if style
                           (cons (token-value tok) style)
                           (token-value tok))
                       (%token-reduction-state-current-args state)))
               (setf (%token-reduction-state-pending-redirect-token state) nil))
             (setf (%token-reduction-state-current-cmd state) (token-value tok)
                   (%token-reduction-state-current-cmd-token state) tok)))
        (:redirect
         (if (%token-reduction-state-current-cmd state)
             (progn
               (%record-missing-redirect-target state)
               (push (cons (token-value tok) nil)
                     (%token-reduction-state-current-args state))
               (setf (%token-reduction-state-pending-redirect-token state) tok))
             (push (%token-diagnostic
                    :missing-command
                    (format nil "Expected command before '~a'" (token-value tok))
                    tok)
                   (%token-reduction-state-errors state))))
        (:error
         (push (cond
                 ((string= "\\" (token-value tok))
                  (%token-diagnostic
                   :trailing-escape
                   "Trailing escape requires continuation"
                   tok))
                 ((and (>= (length (token-value tok)) 2)
                       (string= "<(" (subseq (token-value tok) 0 2)))
                  (%token-diagnostic
                   :unterminated-process-substitution
                   "Unterminated process substitution"
                   tok))
                 (t
                  (%token-diagnostic
                   :unterminated-quote
                   "Unterminated quoted string"
                   tok)))
               (%token-reduction-state-errors state)))
        (t
         (let ((separator (%separator-from-token-type (token-type tok))))
           (if separator
               (%record-token-reduction-separator state separator tok)
               (push (%token-diagnostic
                      :unexpected-token
                      (format nil "Unexpected token: ~a" (token-value tok))
                      tok)
                     (%token-reduction-state-errors state)))))))
    (%flush-token-reduction-command state)
    (values (nreverse (%token-reduction-state-all-cmds state))
            (nreverse (%token-reduction-state-errors state)))))

(defun %parse-structural-diagnostics (cmds last-sep last-sep-token input-length)
  (let ((diagnostics nil)
        (structural-incomplete nil))
    (when (%continuation-separator-p last-sep)
      (setf structural-incomplete t)
      (push (if last-sep-token
                (%token-diagnostic
                 :trailing-continuation
                 (format nil "Expected command after '~a'"
                         (%separator-text last-sep))
                 last-sep-token)
                (make-parse-diagnostic
                 :trailing-continuation
                 "Expected command after continuation operator"
                 input-length
                 input-length))
            diagnostics))
    (when (%unclosed-control-flow-p cmds)
      (setf structural-incomplete t)
      (push (make-parse-diagnostic
             :unclosed-block
             "Expected 'end' to close control-flow block"
             input-length
             input-length)
            diagnostics))
    (dolist (diagnostic (%unexpected-control-flow-diagnostics cmds input-length))
      (push diagnostic diagnostics))
    (values structural-incomplete (nreverse diagnostics))))

(defun parse-tokens (tokens incomplete &key (input-length 0))
  (multiple-value-bind (cmd-list errors)
      (%reduce-token-stream tokens)
    (let* ((cmds (mapcar #'first cmd-list))
           (separators (mapcar #'second cmd-list))
           (separator-tokens (mapcar #'third cmd-list))
           (last-sep (car (last separators)))
           (last-sep-token (car (last separator-tokens)))
           (ast (%build-ast-from-command-list cmd-list)))
      (multiple-value-bind (structural-incomplete structural-diagnostics)
          (%parse-structural-diagnostics cmds last-sep last-sep-token input-length)
        (make-parse-result (group-control-flow ast)
                           (nconc (nreverse errors) structural-diagnostics)
                           (or incomplete structural-incomplete))))))

(defun parse-command-line (input &key (cursor-pos nil))
  (multiple-value-bind (tokens cursor-token incomplete)
      (tokenize input :cursor-pos cursor-pos)
    (declare (ignore cursor-token))
    (if (null tokens)
        (make-parse-result nil nil incomplete)
        (parse-tokens tokens incomplete :input-length (length input)))))
