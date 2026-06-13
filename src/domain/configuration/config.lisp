;;; Shell configuration entity
(in-package #:nshell.domain.configuration)

(defstruct (config (:constructor make-config (&key theme)))
  "Shell configuration aggregating all settings."
  (theme (default-theme) :type theme)
  (prompt-format "[%u@%h %w]> " :type string))

;; config-theme is auto-generated as the accessor for the 'theme' slot
(defun config-prompt (config)
  (config-prompt-format config))

(defun default-config ()
  (make-config :theme (default-theme)))
