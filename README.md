haskell-xkbcommon
===

[libxkbcommon][] is a library by (primarily) Daniel Stone and Ran
Benita which processes keyboard events into characters, taking into
account the keyboard layout and locale settings.

It is intended to, on the one hand, replace existing such
functionality in the X11 project, and on the other, provide a cleaner
base for new display protocol projects such as [Wayland][].

This project provides *incomplete* haskell bindings to libxkbcommon.

Currently, haskell-xkbcommon is a thin layer on top of libxkbcommon,
and this code does not make xkbcommon look much like a Haskell
library: the state processor of libxkbcommon is encoded in the IO
monad.

Documentation
===
See the test suite for example code.

Authors
===
-	Auke Booij

 [libxkbcommon]: http://xkbcommon.org/
 [Wayland]: http://wayland.freedesktop.org/
