;;; Prompt model - pure data structure for prompt rendering
(in-package #:nshell.domain.prompting)

(defstruct (prompt-model (:constructor make-prompt-model (&key hostname cwd exit-code segments)))
  "Pure data model for rendering a shell prompt."
  (hostname "localhost" :type string :read-only t)
  (cwd "/" :type string :read-only t)
  (exit-code 0 :type (or null integer) :read-only t)
  (segments nil :type list :read-only t))

(defstruct (prompt-segment (:constructor make-prompt-segment (text kind)))
  "A segment of the prompt."
  (text "" :type string :read-only t)
  (kind :literal :type keyword :read-only t))

(defun prompt-hostname (pm) (prompt-model-hostname pm))
(defun prompt-cwd (pm) (prompt-model-cwd pm))
(defun prompt-exit-code (pm) (prompt-model-exit-code pm))
(defun prompt-segments (pm) (prompt-model-segments pm))

(defun render-prompt-model (pm)
  "Convert a prompt model into a list of (text . kind) pairs for rendering."
  (let ((segs (prompt-model-segments pm)))
    (when (null segs)
      (setf segs
            (list (make-prompt-segment (prompt-model-hostname pm) :host)
                  (make-prompt-segment " in " :literal)
                  (make-prompt-segment (prompt-model-cwd pm) :path)
                  (make-prompt-segment " " :literal)
                  (make-prompt-segment
                   (if (and (prompt-model-exit-code pm)
                            (not (zerop (prompt-model-exit-code pm))))
                       "✗" "✓")
                   :exit)
                  (make-prompt-segment " > " :literal))))
    (mapcar (lambda (seg)
              (cons (prompt-segment-text seg) (prompt-segment-kind seg)))
            segs)))
