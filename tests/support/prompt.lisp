(in-package #:nshell/test)

(defstruct fake-git-process
  output
  exit-code)

(defun current-display-cwd ()
  "Return the prompt cwd display used by the presentation renderer."
  (let ((cwd (namestring (uiop:getcwd)))
        (home (uiop:getenv "HOME")))
    (if (and home (uiop:string-prefix-p home cwd))
        (concatenate 'string "~" (subseq cwd (length home)))
        cwd)))

(defun current-left-prompt-segments (&key (exit-code 0))
  "Return the left prompt segments for the current test process context."
  (nshell.domain.prompting:render-prompt-model
   (nshell.domain.prompting:make-prompt-model
    :hostname (or (uiop:hostname) "localhost")
    :cwd (current-display-cwd)
    :exit-code exit-code)))

(defun current-left-prompt-width (&key (exit-code 0))
  "Return the visible width of the current left prompt."
  (nshell.presentation::%segments-visible-width
   (current-left-prompt-segments :exit-code exit-code)))

(defun capture-render-prompt (&key (exit-code 0) (terminal-width 80) branch (dirty-p nil))
  "Render the prompt with a deterministic git resolver and return the output string."
  (let ((nshell.domain.prompting:*git-status-resolver*
          (lambda (dir)
            (declare (ignore dir))
            (values branch dirty-p))))
    (with-output-to-string (*standard-output*)
      (nshell.presentation:render-prompt
       (nshell.domain.configuration:default-config)
       exit-code
       :terminal-width terminal-width))))
