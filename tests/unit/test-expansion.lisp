(in-package #:nshell/test)

(def-suite expansion-tests
  :description "Shell expansion tests"
  :in nshell-tests)

(in-suite expansion-tests)

(defun test-expansion-env ()
  (let ((env (nshell.domain.environment:make-environment)))
    (setf env (nshell.domain.environment:env-set env "FOO" "bar" nil))
    (setf env (nshell.domain.environment:env-set env "HOME" "/tmp/nshell-home" nil))
    env))

(test dollar-var-expansion
  "$VAR expands using the shell environment."
  (is (string= "value=bar"
               (nshell.domain.expansion:expand-variables "value=$FOO" (test-expansion-env)))))

(test braced-var-expansion
  "${VAR} expands using the shell environment."
  (is (string= "value=bar"
               (nshell.domain.expansion:expand-variables "value=${FOO}" (test-expansion-env)))))

(test tilde-expansion
  "A leading tilde expands to HOME."
  (is (string= "/tmp/nshell-home/src"
               (nshell.domain.expansion:expand-tilde "~/src" (test-expansion-env)))))

(test double-quoted-expands-variables
  "Double-quoted contents expand $VAR but stay a single field."
  (is (string= "value=bar"
               (nshell.domain.expansion:expand-double-quoted "value=$FOO"
                                                             (test-expansion-env)))))

(test double-quoted-suppresses-globbing
  "Double-quoted contents must not be glob-expanded."
  (is (string= "*"
               (nshell.domain.expansion:expand-double-quoted "*" (test-expansion-env))))
  (is (string= "a*b?c"
               (nshell.domain.expansion:expand-double-quoted "a*b?c"
                                                             (test-expansion-env)))))

(test parameter-default-when-unset
  "${VAR:-word} yields the value when set and the word when unset/empty."
  (let ((env (test-expansion-env)))
    (is (string= "bar" (nshell.domain.expansion:expand-variables "${FOO:-fallback}" env)))
    (is (string= "fallback" (nshell.domain.expansion:expand-variables "${MISSING:-fallback}" env)))
    ;; word is itself expanded
    (is (string= "bar" (nshell.domain.expansion:expand-variables "${MISSING:-$FOO}" env)))))

(test parameter-alternative-when-set
  "${VAR:+word} yields the word only when the variable is set and non-empty."
  (let ((env (test-expansion-env)))
    (is (string= "yes" (nshell.domain.expansion:expand-variables "${FOO:+yes}" env)))
    (is (string= "" (nshell.domain.expansion:expand-variables "${MISSING:+yes}" env)))))

(test parameter-length
  "${#VAR} yields the length of the variable's value."
  (let ((env (test-expansion-env)))
    (is (string= "3" (nshell.domain.expansion:expand-variables "${#FOO}" env)))
    (is (string= "0" (nshell.domain.expansion:expand-variables "${#MISSING}" env)))))

(test parameter-plain-brace-still-works
  "Plain ${VAR} expansion is unchanged by the operator support."
  (is (string= "value=bar"
               (nshell.domain.expansion:expand-variables "value=${FOO}" (test-expansion-env)))))

(test glob-expansion-finds-files
  "A star glob expands to matching files."
  ;; Inject filesystem adapters for DDD purity
  (setf nshell.domain.expansion:*glob-directory-files-fn*
        (lambda (dir) (uiop:directory-files dir)))
  (setf nshell.domain.expansion:*glob-subdirectories-fn*
        (lambda (dir) (uiop:subdirectories dir)))
  (unwind-protect
       (let* ((root (merge-pathnames (format nil "nshell-glob-~a/" (gensym))
                                     (uiop:temporary-directory)))
              (pattern (namestring (merge-pathnames "*.txt" root)))
              (expected (namestring (merge-pathnames "alpha.txt" root))))
         (ensure-directories-exist root)
         (with-open-file (stream expected :direction :output :if-exists :supersede)
           (write-line "alpha" stream))
         (with-open-file (stream (merge-pathnames "beta.log" root)
                                        :direction :output :if-exists :supersede)
           (write-line "beta" stream))
         (is (member expected (nshell.domain.expansion:expand-glob pattern) :test #'string=)))
    ;; Cleanup: restore dynamic variables and delete temp dir
    (setf nshell.domain.expansion:*glob-directory-files-fn* nil)
    (setf nshell.domain.expansion:*glob-subdirectories-fn* nil)
    (handler-case
        (let ((root (probe-file (merge-pathnames "nshell-glob-*/" (uiop:temporary-directory)))))
          (when root (uiop:delete-directory-tree root :validate t)))
      (error ()))))

(test nonexistent-var-expands-empty
  "Undefined variables expand to the empty string."
  (is (string= "prefix--suffix"
               (nshell.domain.expansion:expand-variables "prefix-$NONEXISTENT-suffix"
                                                          (test-expansion-env)))))

(test numeric-var-reference-stays-literal
  "$1 is not treated as a named variable."
  (is (string= "value=$1"
               (nshell.domain.expansion:expand-variables "value=$1"
                                                          (test-expansion-env)))))

(test numeric-var-reference-with-suffix-stays-literal
  "$1foo is kept literal instead of being split into a variable reference."
  (is (string= "value=$1foo"
               (nshell.domain.expansion:expand-variables "value=$1foo"
                                                          (test-expansion-env)))))

(test pbt-valid-variable-names-expand
  "Generated shell variable names expand consistently in both $NAME and ${NAME} forms."
  (check-property (:trials 50)
      ((name (gen-shell-variable-name :min-length 1 :max-length 10)
             #'shrink-prompt-text))
    (let* ((value (concatenate 'string "value-" name))
           (env (nshell.domain.environment:env-set
                 (test-expansion-env) name value nil)))
      (and (string= (nshell.domain.expansion:expand-variables
                     (concatenate 'string "$" name)
                     env)
                    value)
           (string= (nshell.domain.expansion:expand-variables
                     (concatenate 'string "${" name "}")
                     env)
                    value)))))

(test glob-bracket-range-matches-characters
  "Bracket ranges match characters inside the declared span."
  (is (nshell.domain.expansion::glob-match-p "file[0-2].lisp" "file1.lisp"))
  (is (not (nshell.domain.expansion::glob-match-p "file[0-2].lisp" "file9.lisp"))))

(test glob-bracket-negation-matches-outside-set
  "Bracket negation matches characters outside the declared set."
  (is (nshell.domain.expansion::glob-match-p "file[!0-2].lisp" "file9.lisp"))
  (is (not (nshell.domain.expansion::glob-match-p "file[!0-2].lisp" "file1.lisp")))
  (is (nshell.domain.expansion::glob-match-p "file[^ab].lisp" "filez.lisp"))
  (is (not (nshell.domain.expansion::glob-match-p "file[^ab].lisp" "filea.lisp"))))

(test glob-unclosed-bracket-matches-literal
  "An unclosed bracket is treated as a literal character."
  (is (nshell.domain.expansion::glob-match-p "file[1" "file[1"))
  (is (not (nshell.domain.expansion::glob-match-p "file[1" "file11"))))

(test pbt-glob-ranges-match-generated-members
  "Generated characters inside a bracket range always match that range."
  (check-property (:trials 50)
      ((start (gen-in-range 97 122) nil)
       (end (gen-in-range 97 122) nil))
    (let* ((lo (code-char (min start end)))
           (hi (code-char (max start end)))
           (mid (code-char (floor (+ (char-code lo) (char-code hi)) 2))))
      (nshell.domain.expansion::glob-match-p (format nil "[~c-~c]" lo hi)
                                             (string mid)))))

(test pbt-glob-negated-ranges-reject-generated-members
  "Generated characters inside a negated bracket range never match that range."
  (check-property (:trials 50)
      ((start (gen-in-range 97 122) nil)
       (end (gen-in-range 97 122) nil))
    (let* ((lo (code-char (min start end)))
           (hi (code-char (max start end)))
           (mid (code-char (floor (+ (char-code lo) (char-code hi)) 2))))
      (not (nshell.domain.expansion::glob-match-p (format nil "[!~c-~c]" lo hi)
                                                  (string mid))))))

(test abbreviation-domain-finds-token-before-cursor
  (multiple-value-bind (token start end found-p)
      (nshell.domain.abbreviation:abbreviation-target-before-cursor
       "echo|gco" 8)
    (is (not (null found-p)))
    (is (string= "gco" token))
    (is (= 5 start))
    (is (= 8 end))))

(test abbreviation-domain-expands-token-before-cursor
  (multiple-value-bind (buffer cursor expanded-p)
      (nshell.domain.abbreviation:expand-abbreviation
       "echo gco tail"
       8
       (lambda (token)
         (when (string= token "gco")
           "git checkout")))
    (is (not (null expanded-p)))
    (is (string= "echo git checkout tail" buffer))
    (is (= 17 cursor))))

(test abbreviation-domain-command-position-detects-command-starts
  (is (nshell.domain.abbreviation:abbreviation-command-position-p "gco" 0))
  (is (nshell.domain.abbreviation:abbreviation-command-position-p "echo hi; gco" 9))
  (is (nshell.domain.abbreviation:abbreviation-command-position-p "echo hi | gco" 10))
  (is (not (nshell.domain.abbreviation:abbreviation-command-position-p
            "echo gco" 5)))
  (is (not (nshell.domain.abbreviation:abbreviation-command-position-p
            "cat < gco" 6))))

(test abbreviation-domain-respects-command-position-expansions
  (let ((abbr (nshell.domain.abbreviation:make-abbreviation
               :expansion "git checkout"
               :position :command)))
    (multiple-value-bind (buffer cursor expanded-p)
        (nshell.domain.abbreviation:expand-abbreviation
         "gco"
         3
         (lambda (token)
           (when (string= token "gco")
             abbr)))
      (is (not (null expanded-p)))
      (is (string= "git checkout" buffer))
      (is (= 12 cursor)))
    (multiple-value-bind (buffer cursor expanded-p)
        (nshell.domain.abbreviation:expand-abbreviation
         "echo gco"
         8
         (lambda (token)
           (when (string= token "gco")
             abbr)))
      (is (not expanded-p))
      (is (string= "echo gco" buffer))
      (is (= 8 cursor)))))

(test abbreviation-domain-expands-after-leading-space-command-position
  (let ((abbr (nshell.domain.abbreviation:make-abbreviation
               :expansion "git checkout"
               :position :command)))
    (multiple-value-bind (buffer cursor expanded-p)
        (nshell.domain.abbreviation:expand-abbreviation
         "  gco"
         5
         (lambda (token)
           (when (string= token "gco")
             abbr)))
      (is (not (null expanded-p)))
      (is (string= "  git checkout" buffer))
      (is (= 14 cursor)))))

(test abbreviation-domain-respects-escaped-space
  (multiple-value-bind (buffer cursor expanded-p)
      (nshell.domain.abbreviation:expand-abbreviation
       "echo foo\\ gco"
       13
       (lambda (token)
         (when (string= token "gco")
           "git checkout")))
    (is (not expanded-p))
    (is (string= "echo foo\\ gco" buffer))
    (is (= 13 cursor))))

(test abbreviation-domain-does-not-expand-quoted-token
  (multiple-value-bind (buffer cursor expanded-p)
      (nshell.domain.abbreviation:expand-abbreviation
       "echo \"gco\""
       10
       (lambda (token)
         (when (string= token "\"gco\"")
           "git checkout")))
    (is (not expanded-p))
    (is (string= "echo \"gco\"" buffer))
    (is (= 10 cursor))))

(test abbreviation-domain-allows-escaped-quote-content
  (multiple-value-bind (buffer cursor expanded-p)
      (nshell.domain.abbreviation:expand-abbreviation
       "foo\\\"bar"
       8
       (lambda (token)
         (when (string= token "foo\\\"bar")
           "git checkout")))
      (is (not (null expanded-p)))
    (is (string= "git checkout" buffer))
    (is (= 12 cursor))))

(test pbt-abbreviation-domain-expands-token-exactly-at-cursor
  (check-property (:trials 50)
      ((prefix (gen-shell-command :min-words 1 :max-words 3 :max-word-length 6)
               #'shrink-prompt-text)
       (token (gen-shell-word :min-length 1 :max-length 8)
              #'shrink-prompt-text)
       (suffix (gen-shell-command :min-words 1 :max-words 3 :max-word-length 6)
               #'shrink-prompt-text))
    (let* ((expansion (concatenate 'string "expanded-" token))
           (buffer (concatenate 'string prefix " " token " " suffix))
           (cursor (+ (length prefix) 1 (length token))))
      (multiple-value-bind (new-buffer new-cursor expanded-p)
          (nshell.domain.abbreviation:expand-abbreviation
           buffer cursor
           (lambda (candidate)
             (when (string= candidate token)
               expansion)))
        (and expanded-p
             (string= (concatenate 'string prefix " " expansion " " suffix)
                      new-buffer)
             (= (+ (length prefix) 1 (length expansion))
                new-cursor))))))
