(in-package #:nshell/test)

(in-suite terminal-integration-tests)

(test terminal-ansi-emits-advanced-control-sequences
  "Advanced terminal mode helpers emit standard ANSI control sequences."
  (let ((output (with-output-to-string (stream)
                  (nshell.infrastructure.terminal:ansi-hide-cursor stream)
                  (nshell.infrastructure.terminal:ansi-show-cursor stream)
                  (nshell.infrastructure.terminal:ansi-enable-bracketed-paste stream)
                  (nshell.infrastructure.terminal:ansi-disable-bracketed-paste stream)
                  (nshell.infrastructure.terminal:ansi-enable-sgr-mouse stream)
                  (nshell.infrastructure.terminal:ansi-disable-sgr-mouse stream)
                  (nshell.infrastructure.terminal:ansi-enable-alternate-screen stream)
                  (nshell.infrastructure.terminal:ansi-disable-alternate-screen stream))))
    (is (search (format nil "~C[?25l" #\Esc) output))
    (is (search (format nil "~C[?25h" #\Esc) output))
    (is (search (format nil "~C[?2004h" #\Esc) output))
    (is (search (format nil "~C[?2004l" #\Esc) output))
    (is (search (format nil "~C[?1000h~C[?1006h" #\Esc #\Esc) output))
    (is (search (format nil "~C[?1006l~C[?1000l" #\Esc #\Esc) output))
    (is (search (format nil "~C[?1049h" #\Esc) output))
    (is (search (format nil "~C[?1049l" #\Esc) output))))
