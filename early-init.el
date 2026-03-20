;;; early-init.el --- Early startup configuration -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Prevent package.el from loading packages too early
;; init.el will control package initialization
;; --------------------------------------------------

(setq package-enable-at-startup nil)

;; --------------------------------------------------
;; Improve startup performance
;; Increase GC threshold during startup
;; --------------------------------------------------

(setq gc-cons-threshold (* 50 1000 1000))

;; --------------------------------------------------
;; Disable unnecessary UI elements early
;; Prevents GUI flicker during startup
;; --------------------------------------------------

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; --------------------------------------------------
;; Avoid frame resizing during startup
;; improves startup speed for GUI Emacs
;; --------------------------------------------------

(setq frame-inhibit-implied-resize t)

;; --------------------------------------------------
;; Disable startup message and scratch message
;; --------------------------------------------------

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; --------------------------------------------------
;; Faster native compilation warnings handling
;; (Emacs 28+)
;; --------------------------------------------------

(setq native-comp-async-report-warnings-errors nil)

;; End of early-init.el
