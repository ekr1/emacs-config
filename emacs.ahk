; emacs.ahk:

#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

; ! Alt
; ^ Control
; + Shift

#IfWinActive, zeus emacs

end:: Send, ^xpe

^right:: Send, ^xpcr
^left:: Send, ^xpcl
^up:: Send, ^xpcu
^down:: Send, ^xpcd
^end:: Send, ^xpce
^home:: Send, ^xpch
