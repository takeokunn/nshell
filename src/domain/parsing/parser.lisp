(in-package #:nshell.domain.parsing)

(defstruct (parse-result (:constructor make-parse-result (ast &optional errors incomplete)))
  (ast nil :type (or null ast-node))
  (errors nil :type list)
  (incomplete nil :type boolean))

(defun parse-complete-p (result)
  (and (parse-result-ast result)
       (null (parse-result-errors result))
       (not (parse-result-incomplete result))))

(defun parse-errors (result)
  (parse-result-errors result))

(defun parse-command-line (input &key (cursor-pos nil))
  (multiple-value-bind (tokens cursor-token incomplete)
      (tokenize input :cursor-pos cursor-pos)
    (declare (ignore cursor-token))
    (if (null tokens)
        (make-parse-result nil nil incomplete)
        (parse-tokens tokens incomplete))))

(defun parse-tokens (tokens incomplete)
  (let ((all-cmds '())             ; flat list of (cmd . next-separator) pairs
        (current-args '())
        (current-cmd nil)
        (pending-sep nil)          ; separator FOLLOWING the current command
        (errors '()))
    (labels ((flush-command ()
               (when current-cmd
                 (push (cons (make-command-node current-cmd (nreverse current-args)) pending-sep)
                       all-cmds)
                 (setf current-cmd nil current-args nil pending-sep nil))))
      (dolist (tok tokens)
        (case (token-type tok)
          (:word
           (if current-cmd
               (push (if (token-quoted-p tok)
                         (cons (token-value tok) t)
                         (token-value tok))
                     current-args)
               (setf current-cmd (token-value tok))))
          (:pipe (setf pending-sep :pipe) (flush-command))
          (:semicolon (setf pending-sep :semi) (flush-command))
          (:ampersand (setf pending-sep :amp) (flush-command))
          (:and (setf pending-sep :and) (flush-command))
          (:or (setf pending-sep :or) (flush-command))
          (:redirect (push (cons (token-value tok) nil) current-args))
          (:error (push (format nil "Parse error near: ~a" (token-value tok)) errors))
          (t (push (format nil "Unexpected token: ~a" (token-value tok)) errors))))
      (flush-command))
    (let* ((cmd-list (nreverse all-cmds))
           (cmds (mapcar #'car cmd-list))
           (separators (mapcar #'cdr cmd-list))
           (ast (cond
                  ((null cmds) nil)
                  ((= (length cmds) 1)
                   ;; Trailing & / ; / | for single command → preserve as sequence
                   (if (eq :amp (first separators))
                       (make-sequence-node cmds '(:amp))
                       (first cmds)))
                  ;; All pipe: single pipeline
                  ((every (lambda (s) (eq :pipe s)) (butlast separators))
                   (make-pipeline-node cmds))
                  ;; All non-pipe: flat sequence
                  ((every (lambda (s) (not (eq :pipe s))) (butlast separators))
                   (make-sequence-node cmds (butlast separators)))
                  ;; Mixed: group consecutive pipe-connected commands into pipeline-nodes
                  (t
                   (let ((seq-cmds nil) (seq-seps nil) (pipe-group nil))
                     (labels ((flush-pipe-group ()
                                (let ((n (length pipe-group)))
                                  (when (> n 0)
                                    (push (if (= n 1) (first pipe-group)
                                              (make-pipeline-node (nreverse pipe-group)))
                                          seq-cmds)
                                    (setf pipe-group nil)))))
                       (loop for cmd in cmds
                             for i from 0
                             for sep = (nth i separators)
                             do (push cmd pipe-group)
                                (when (and sep (not (eq sep :pipe)))
                                  (flush-pipe-group)
                                  (push sep seq-seps)))
                       (flush-pipe-group))
                     (make-sequence-node (nreverse seq-cmds) (nreverse seq-seps)))))))
      (make-parse-result ast errors incomplete))))
