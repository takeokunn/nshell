(in-package #:nshell/test)

(in-suite prompt-tests)

(test render-prompt-truncates-right-prompt-to-current-terminal-width
  "The presentation prompt uses the supplied terminal width for right prompt alignment."
  (let* ((terminal-width (+ (current-left-prompt-width) 2 4))
         (output (capture-render-prompt :terminal-width terminal-width
                                        :branch "abcdef")))
    (is (search "abcd" output))
    (is (not (search "abcdef" output)))))

(test render-prompt-restores-cursor-after-right-prompt
  "Right prompt rendering should leave the cursor after the left prompt for input text."
  (let ((output (capture-render-prompt :terminal-width (+ (current-left-prompt-width) 10)
                                       :branch "main")))
    (is (search (format nil "~C7" #\Esc) output))
    (is (search (format nil "~C8" #\Esc) output))))

(test render-prompt-renders-time-in-right-prompt
  "The presentation layer should surface the right-prompt time segment."
  (let ((nshell.domain.prompting:*prompt-time-resolver*
          (lambda ()
            "12:34")))
    (let ((output (capture-render-prompt :terminal-width (+ (current-left-prompt-width) 12)
                                         :branch nil)))
      (is (search "12:34" output)))))

(test render-prompt-renders-duration-in-right-prompt
  "The presentation layer should surface the last-command duration segment."
  (let ((output (capture-render-prompt :terminal-width (+ (current-left-prompt-width) 12)
                                       :branch nil
                                       :duration-ms 123)))
    (is (search "123ms" output))))

(test render-prompt-returns-left-visible-width
  "The prompt renderer reports the left prompt width for edit-buffer cursor placement."
  (multiple-value-bind (output results)
      (call-render-prompt :terminal-width 80)
    (declare (ignore output))
    (is (= (current-left-prompt-width)
           (first results)))))

(test home-prefix-only-matches-path-boundaries
  "Home-directory shortening should not trigger on plain string prefixes."
  (is (nshell.presentation::%home-prefix-p "/Users/take" "/Users/take"))
  (is (nshell.presentation::%home-prefix-p "/Users/take" "/Users/take/projects"))
  (is (not (nshell.presentation::%home-prefix-p "/Users/take" "/Users/takefoo"))))
