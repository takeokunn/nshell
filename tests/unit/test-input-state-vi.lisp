(in-package #:nshell/test)

(in-suite input-state-tests)

;;; Vi-mode reducer behavior. ESC only enters vi normal mode when
;;; *VI-MODE-ENABLED* is true, so each test binds it explicitly.

(defmacro with-vi-mode (&body body)
  `(let ((nshell.presentation::*vi-mode-enabled* t))
     ,@body))

(test vi-escape-enters-command-mode-and-moves-left
  (with-vi-mode
    (let ((state (input-state :buffer "echo hi" :cursor-pos 7)))
      (with-reduced-input-state (cmd) (reduce-once state :escape)
        (is-input-state cmd :mode :vi-command :cursor-pos 6)))))

(test vi-escape-is-inert-without-vi-mode
  (let ((state (input-state :buffer "echo hi" :cursor-pos 7)))
    (with-reduced-input-state (after) (reduce-once state :escape)
      (is-input-state after :mode :insert :cursor-pos 7))))

(test vi-hl-motion-and-bounds
  (with-vi-mode
    (let ((cmd (reduce-once-state (input-state :buffer "abc" :cursor-pos 1) :escape)))
      ;; ESC moved cursor 1 -> 0; l advances, h retreats, both clamped.
      (with-reduced-input-state (r1) (reduce-once cmd :char #\l)
        (is-input-state r1 :cursor-pos 1)
        (with-reduced-input-state (r2) (reduce-once r1 :char #\l)
          (is-input-state r2 :cursor-pos 2)
          ;; last column for "abc" in command mode is 2; cannot pass it.
          (with-reduced-input-state (r3) (reduce-once r2 :char #\l)
            (is-input-state r3 :cursor-pos 2)
            (with-reduced-input-state (r4) (reduce-once r3 :char #\h)
              (is-input-state r4 :cursor-pos 1))))))))

(test vi-line-jumps-0-and-dollar
  (with-vi-mode
    (let ((cmd (reduce-once-state (input-state :buffer "hello" :cursor-pos 2) :escape)))
      (with-reduced-input-state (s0) (reduce-once cmd :char #\0)
        (is-input-state s0 :cursor-pos 0))
      (with-reduced-input-state (se) (reduce-once cmd :char #\$)
        (is-input-state se :cursor-pos 4)))))

(test vi-i-returns-to-insert-mode
  (with-vi-mode
    (let ((cmd (reduce-once-state (input-state :buffer "abc" :cursor-pos 0) :escape)))
      (with-reduced-input-state (ins) (reduce-once cmd :char #\i)
        (is-input-state ins :mode :insert))
      ;; A enters insert at end of line.
      (with-reduced-input-state (app) (reduce-once cmd :char #\A)
        (is-input-state app :mode :insert :cursor-pos 3)))))

(test vi-x-deletes-character-under-cursor
  (with-vi-mode
    (let ((cmd (reduce-once-state (input-state :buffer "abc" :cursor-pos 0) :escape)))
      (with-reduced-input-state (del) (reduce-once cmd :char #\x)
        (is-input-state del :buffer "bc" :mode :vi-command)))))

(test vi-dd-clears-line-and-cc-enters-insert
  (with-vi-mode
    (let ((cmd (reduce-once-state (input-state :buffer "hello world" :cursor-pos 3) :escape)))
      ;; dd : two keystrokes delete the whole line.
      (with-reduced-input-state (pend) (reduce-once cmd :char #\d)
        (is-input-state pend :mode :vi-d)
        (with-reduced-input-state (cleared) (reduce-once pend :char #\d)
          (is-input-state cleared :buffer "" :mode :vi-command)))
      ;; cc : clear line and drop into insert mode.
      (with-reduced-input-state (cpend) (reduce-once cmd :char #\c)
        (with-reduced-input-state (changed) (reduce-once cpend :char #\c)
          (is-input-state changed :buffer "" :mode :insert))))))

(test vi-D-kills-to-end-of-line
  (with-vi-mode
    (let* ((base (input-state :buffer "hello world" :cursor-pos 6))
           ;; ESC moves cursor 6 -> 5 (the space); place it on 'w' via l.
           (cmd (reduce-once-state base :escape))
           (on-w (reduce-once-state cmd :char #\l)))
      (with-reduced-input-state (killed) (reduce-once on-w :char #\D)
        (is-input-state killed :buffer "hello " :mode :vi-command)))))

(test vi-j-k-emit-history-navigation
  (with-vi-mode
    (let ((cmd (reduce-once-state (input-state :buffer "x" :cursor-pos 0) :escape)))
      (multiple-value-bind (s out) (reduce-once cmd :char #\k)
        (declare (ignore s))
        (is (eq :history-prev out)))
      (multiple-value-bind (s out) (reduce-once cmd :char #\j)
        (declare (ignore s))
        (is (eq :history-next out))))))
