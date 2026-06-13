(in-package #:nshell.presentation)

(defun segment-kind->role (kind)
  "Map prompt segment kind to highlight role for theme lookup."
  (case kind
    (:host :prompt-host)
    (:path :prompt-path)
    (:exit :prompt-ok)
    (:exit-error :prompt-error)
    (:literal :normal)
    (:git :prompt-path)
    (:time :comment)
    (t :normal)))

(defun render-prompt (config last-exit)
  "Render the left prompt with theme colors."
  (let* ((theme (nshell.domain.configuration:config-theme config))
         (cwd (namestring (uiop:getcwd)))
         (home (uiop:getenv "HOME"))
         (display-cwd (if (and home (uiop:string-prefix-p home cwd))
                         (concatenate 'string "~" (subseq cwd (length home)))
                         cwd))
         (pm (nshell.domain.prompting:make-prompt-model
              :hostname (or (uiop:hostname) "localhost")
              :cwd display-cwd
              :exit-code last-exit))
         (segments (nshell.domain.prompting:render-prompt-model pm)))
    (dolist (seg segments)
      (let ((text (car seg))
            (kind (cdr seg)))
        (format t "~a~a~C[0m"
                (theme-color->ansi theme (segment-kind->role kind))
                text
                #\Esc)))
    ;; Right prompt
    (let ((right-segs (nshell.domain.prompting:render-right-prompt-model pm)))
      (when right-segs
        (let ((right-text
               (with-output-to-string (s)
                 (dolist (seg right-segs)
                   (let ((text (car seg))
                         (kind (cdr seg)))
                     (format s "~a~a~C[0m"
                             (theme-color->ansi theme (segment-kind->role kind))
                             text
                             #\Esc))))))
          ;; Render right-aligned: calculate space, write right prompt at end
          ;; Simple approach: output spaces then right prompt
          (let* ((term-width 80) ;; TODO: get actual terminal width
                 (left-width (reduce #'+ (mapcar (lambda (s) (length (car s))) segments)))
                 (right-width (length (nshell.domain.prompting:render-right-prompt-model pm)))
                 (padding (- term-width left-width right-width 2)))
            (when (> padding 0)
              (format t "~C[~dC~a" #\Esc padding right-text))))))
    (finish-output)))

(defun render-input-line (line offset)
  (declare (ignore offset))
  (format t "~a" line))
