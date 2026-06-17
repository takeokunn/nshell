(in-package #:nshell/test)

(defun input-key-event (type &optional char number data)
  (nshell.domain.input:make-key-event type char number data))

(defun reduce-once (state type &optional char number data)
  (nshell.presentation:reduce-input-state
   state
   (input-key-event type char number data)))

(defun reduce-once-state (state type &optional char number data)
  (nth-value 0 (reduce-once state type char number data)))

(defmacro with-kill-then-yank ((killed-var yanked-var
                                &optional kill-output-var yank-output-var)
                               state
                               kill-type
                               &body body)
  (let ((killed-state (gensym "KILLED-STATE"))
        (kill-output (gensym "KILL-OUTPUT"))
        (yanked-state (gensym "YANKED-STATE"))
        (yank-output (gensym "YANK-OUTPUT")))
    `(multiple-value-bind (,@(list killed-state)
                           ,@(when kill-output-var (list kill-output)))
         (reduce-once ,state ,kill-type)
       (multiple-value-bind (,@(list yanked-state)
                             ,@(when yank-output-var (list yank-output)))
           (reduce-once ,killed-state :ctrl-y)
         (let ((,killed-var ,killed-state)
               (,yanked-var ,yanked-state)
               ,@(when kill-output-var
                   `((,kill-output-var ,kill-output)))
               ,@(when yank-output-var
                   `((,yank-output-var ,yank-output))))
           ,@body)))))

(defmacro with-reduced-input-state ((state-var &optional output-var) reduction-form &body body)
  (if output-var
      `(multiple-value-bind (,state-var ,output-var)
           ,reduction-form
         (declare (ignorable ,state-var ,output-var))
         ,@body)
      (let ((ignored-output (gensym "OUTPUT")))
        `(multiple-value-bind (,state-var ,ignored-output)
             ,reduction-form
           (declare (ignorable ,state-var ,ignored-output))
           ,@body))))

(defmacro with-reduced-input-states (state steps &body body)
  (labels ((expand (current-state remaining)
             (if (endp remaining)
                 `(progn ,@body)
                 (destructuring-bind ((state-var &optional output-var)
                                      event-type
                                      &rest event-args)
                     (first remaining)
                   (let ((ignored-output (gensym "OUTPUT")))
                     `(multiple-value-bind (,@(list state-var)
                                            ,@(if output-var
                                                  (list output-var)
                                                  (list ignored-output)))
                          (reduce-once ,current-state ,event-type ,@event-args)
                        (declare (ignorable ,state-var
                                            ,@(if output-var
                                                  (list output-var)
                                                  (list ignored-output))))
                        ,(expand state-var (rest remaining))))))))
    (expand state steps)))

(defun input-state (&rest initargs)
  (apply #'nshell.presentation:make-input-state initargs))

(defun history-search-state (&key
                             (buffer "")
                             cursor-pos
                             (query "")
                             original-buffer
                             original-cursor
                             (index 0)
                             completion-index
                             completion-base-buffer
                             completion-base-cursor
                             last-candidates
                             suggestion)
  (let ((base-buffer (or original-buffer buffer)))
    (apply #'input-state
           :mode :search
           :buffer buffer
           :cursor-pos (or cursor-pos (length buffer))
           :search-query query
           :search-original-buffer base-buffer
           :search-original-cursor (or original-cursor (length base-buffer))
           :search-index index
           (append (when completion-index
                     (list :completion-index completion-index))
                   (when completion-base-buffer
                     (list :completion-base-buffer completion-base-buffer))
                   (when completion-base-cursor
                     (list :completion-base-cursor completion-base-cursor))
                   (when last-candidates
                     (list :last-candidates last-candidates))
                   (when suggestion
                     (list :suggestion suggestion))))))

(defmacro with-expected-input-state-reduction ((state-var output-var)
                                               state-form
                                               reduction-form
                                               expected-output
                                               state-args
                                               &body body)
  `(let ((state ,state-form))
     (declare (ignorable state))
     (with-reduced-input-state (,state-var ,output-var)
         ,reduction-form
       (declare (ignorable ,state-var ,output-var))
       (is-input-state ,state-var ,@state-args)
       (is (eq ,expected-output ,output-var))
       ,@body)))

(defmacro with-expected-suggestion-reduction ((state-var output-var)
                                              (buffer cursor-pos suggestion event)
                                              expected-buffer
                                              expected-cursor-pos
                                              expected-suggestion
                                              expected-output
                                              &body body)
  `(with-expected-input-state-reduction (,state-var ,output-var)
       (input-state
        :buffer ,buffer
        :cursor-pos ,cursor-pos
        :suggestion ,suggestion)
       (reduce-once state ,event)
       ,expected-output
       (:buffer ,expected-buffer
        :cursor-pos ,expected-cursor-pos
        :suggestion ,expected-suggestion)
     ,@body))

(defun read-key-events-from-string (text)
  (let ((*standard-input* (make-string-input-stream text)))
    (loop for event = (nshell.infrastructure.terminal:read-key-event)
          while event
          collect event)))

(defun single-key-event-from-string (text)
  (first (read-key-events-from-string text)))

(defun esc-sequence (suffix)
  (concatenate 'string (string #\Esc) suffix))

(defun apply-key-events-to-input-state (state events)
  (loop with current = state
        for event in events
        do (with-reduced-input-state (next-state)
               (nshell.presentation:reduce-input-state current event)
             (setf current next-state))
        finally (return current)))

(defun is-maybe-string (expected actual)
  (if expected
      (is (string= expected actual))
      (is (null actual))))

(defun is-maybe-number (expected actual)
  (if expected
      (is (= expected actual))
      (is (null actual))))

(defmacro is-input-state (state &key
                          ((:buffer buffer) nil buffer-p)
                          ((:cursor-pos cursor-pos) nil cursor-pos-p)
                          ((:completion-index completion-index) nil completion-index-p)
                          ((:suggestion suggestion) nil suggestion-p)
                          ((:completion-base-buffer completion-base-buffer)
                           nil
                           completion-base-buffer-p)
                          ((:completion-base-cursor completion-base-cursor)
                           nil
                           completion-base-cursor-p)
                          ((:last-candidates last-candidates) nil last-candidates-p)
                          ((:kill-ring kill-ring) nil kill-ring-p)
                          ((:last-argument-start last-argument-start)
                           nil
                           last-argument-start-p)
                          ((:last-argument-end last-argument-end)
                           nil
                           last-argument-end-p)
                          ((:last-argument-index last-argument-index)
                           nil
                           last-argument-index-p))
  (let ((state-var (gensym "STATE")))
    `(let ((,state-var ,state))
       ,@(when buffer-p
           `((is (string= ,buffer
                          (nshell.presentation:input-state-buffer ,state-var)))))
       ,@(when cursor-pos-p
           `((is (= ,cursor-pos
                    (nshell.presentation:input-state-cursor-pos ,state-var)))))
       ,@(when completion-index-p
           `((is (= ,completion-index
                    (nshell.presentation:input-state-completion-index ,state-var)))))
       ,@(when suggestion-p
           `((is-maybe-string
              ,suggestion
              (nshell.presentation:input-state-suggestion ,state-var))))
       ,@(when completion-base-buffer-p
           `((is-maybe-string
              ,completion-base-buffer
              (nshell.presentation:input-state-completion-base-buffer ,state-var))))
       ,@(when completion-base-cursor-p
           `((is-maybe-number
              ,completion-base-cursor
              (nshell.presentation:input-state-completion-base-cursor ,state-var))))
       ,@(when last-candidates-p
           `((is (equal ,last-candidates
                        (nshell.presentation:input-state-last-candidates ,state-var)))))
       ,@(when kill-ring-p
           `((is (equal ,kill-ring
                        (nshell.presentation:input-state-kill-ring ,state-var)))))
       ,@(when last-argument-start-p
           `((is-maybe-number
              ,last-argument-start
              (nshell.presentation:input-state-last-argument-start ,state-var))))
       ,@(when last-argument-end-p
           `((is-maybe-number
              ,last-argument-end
              (nshell.presentation:input-state-last-argument-end ,state-var))))
       ,@(when last-argument-index-p
           `((is-maybe-number
              ,last-argument-index
              (nshell.presentation:input-state-last-argument-index ,state-var)))))))

(defmacro is-completion-session-cleared (state)
  `(is-input-state ,state
                   :completion-index -1
                   :completion-base-buffer nil
                   :completion-base-cursor nil
                   :last-candidates nil
                   :suggestion nil))
