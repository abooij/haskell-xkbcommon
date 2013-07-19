haskell-xkbcommon
===

[libxkbcommon][] is a library by (primarily) Daniel Stone which processes keyboard events into
characters, taking into account the keyboard layout and locale settings.

It is intended to, on the one hand, replace existing such functionality in the X11 project,
and on the other, provide a cleaner base for new display protocol projects such as [Wayland][].

These are the haskell bindings of libxkbcommon.

Note that currently, haskell-xkbcommon is a very thin layer on top of libxkbcommon.

I need to read McAllister "High-level FFI in Haskell" and apply those ideas.

Authors
===
-	Auke Booij

 [libxkbcommon]: http://xkbcommon.org/
 [Wayland]: http://wayland.freedesktop.org/
