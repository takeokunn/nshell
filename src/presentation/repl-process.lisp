;;; REPL process and background job helpers
(in-package #:nshell.presentation)

(defun register-background-proc (job-id proc)
  (setf (gethash job-id *proc-registry*) proc))

(defun %background-proc-list (entry)
  (cond
    ((null entry) nil)
    ((listp entry) entry)
    (t (list entry))))

(defun %process-alive-p (proc)
  (sb-ext:process-alive-p proc))

(defun %process-exit-code (proc)
  (sb-ext:process-exit-code proc))

(defun %background-procs-alive-p (entry)
  (some #'%process-alive-p (%background-proc-list entry)))

(defun %background-procs-exit-code (entry)
  (let ((proc (car (last (%background-proc-list entry)))))
    (or (and proc (%process-exit-code proc)) 0)))

(defun reap-background-jobs ()
  (let ((completed-jobs nil))
    (maphash (lambda (jid entry)
               (when (and entry (not (%background-procs-alive-p entry)))
                 (nshell.domain.job-control:monitor-update
                  nshell.application:*job-monitor* jid :completed
                  (%background-procs-exit-code entry))
                 (push jid completed-jobs)))
             *proc-registry*)
    (dolist (jid completed-jobs)
      (remhash jid *proc-registry*))))

(defun extract-redirects (args)
  (let ((clean nil)
        (redirects nil)
        (index 0))
    (loop while (< index (length args))
          for value = (nth index args)
          do (cond
               ((and (string= value ">")
                     (< (1+ index) (length args)))
                (push (cons :> (nth (1+ index) args)) redirects)
                (incf index 2))
               ((and (string= value ">>")
                     (< (1+ index) (length args)))
                (push (cons :>> (nth (1+ index) args)) redirects)
                (incf index 2))
               ((and (string= value "<")
                     (< (1+ index) (length args)))
                (push (cons :< (nth (1+ index) args)) redirects)
                (incf index 2))
               (t
                (push value clean)
                (incf index))))
    (values (nreverse clean) (nreverse redirects))))

(defun apply-redirects (redirects)
  (dolist (redirect redirects)
    (let ((op (car redirect))
          (target (cdr redirect)))
      (case op
        (:> (nshell.infrastructure.acl:redirect-output target :supersede))
        (:>> (nshell.infrastructure.acl:redirect-output target :append))
        (:< (nshell.infrastructure.acl:redirect-input target))))))
