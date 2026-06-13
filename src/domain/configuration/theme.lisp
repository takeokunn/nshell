;;; Theme value object - fish-style color configuration
(in-package #:nshell.domain.configuration)

(defstruct (theme (:constructor make-theme (&key
                                            (name "default")
                                            (colors (make-hash-table :test #'eq)))))
  "A shell theme defining named colors for different syntax elements.
Fish-inspired color names: fish_color_command, fish_color_param, etc."
  (name "default" :type string :read-only t)
  (colors (make-hash-table :test #'eq) :type hash-table :read-only t))

(defun theme-color (theme key)
  "Get color for KEY (keyword) from THEME, or nil if not defined."
  (gethash key (theme-colors theme)))

(defun theme-set-color (theme key value)
  (setf (gethash key (theme-colors theme)) value)
  theme)

;; theme-name is the auto-generated struct accessor, no need to redefine

(defun default-theme ()
  "Create the default fish-inspired theme."
  (let ((th (make-theme :name "nshell-default")))
    (theme-set-color th :normal "00FF00")
    (theme-set-color th :command "00AFFF")
    (theme-set-color th :param "00AFFF")
    (theme-set-color th :comment "737373")
    (theme-set-color th :error "FF0000")
    (theme-set-color th :operator "FFFF00")
    (theme-set-color th :quote "FFA500")
    (theme-set-color th :redirection "00AFFF")
    (theme-set-color th :autosuggestion "555555")
    (theme-set-color th :search-match "FFFF00")
    (theme-set-color th :selection "FFFFFF")
    (theme-set-color th :prompt-host "00AFFF")
    (theme-set-color th :prompt-path "00FF00")
    (theme-set-color th :prompt-error "FF0000")
    (theme-set-color th :prompt-ok "00FF00")
    th))
