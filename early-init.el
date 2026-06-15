;;; early-init.el --- pre-init configuration -*- lexical-binding: t; -*-

; raise GC threshold during startup for faster init; restored in init.el
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

; using straight instead of package.el
(setq package-enable-at-startup nil)

; disable UI chrome before the first frame is drawn (avoids flash at startup)
(menu-bar-mode -1)
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
