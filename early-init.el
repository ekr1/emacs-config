;;; early-init.el --- pre-init configuration -*- lexical-binding: t; -*-

; using straight instead of package.el
(setq package-enable-at-startup nil)

; disable UI chrome before the first frame is drawn (avoids flash at startup)
(menu-bar-mode -1)
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
