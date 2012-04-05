ido-speedup-hack.el
===================

Speed optimisations for Emacs IDO mode. It makes ido completion fast
and interactive even with large sets of completions (eg. list of all
commands in M-x, or large Info index files like Elisp or Emacs
manual).

To achieve maximum speed, download following optimisations:

* [ido-mode-el][] original ido.el 
* [ido-better-flex][] optional package with better flexible suggestions.

Optimisations
-------------
ido-speed-hack increases ido performance in several ways:

* function inlining (defsubst)
* disabling ido-case-fold for some completions (all commands are lowercase anyway)
* prunning collections based on character bitmaps
* caching character bitmaps during completion or for same completions
* caching intermediate completion lists when adding new characters

Last three optimisations works only when regular expressions are disabled.

Character bitmaps
-----------------
List of completions are prefiltered based on character bitmaps
Character bitmap is computed in function ido-string-to-bitmask. 

Each character from a .. z (regardless case) is assigned one bit.
Numbers, special characters - and * has also one bit assigned. 30 bits
are enought to store all flags.

Character bitmap of a string is or of each individual string
characters. 

Before ido starts to examine completion list, ido-speed-hack
prefilters the list and removes all words which miss some characters
in ido-text. The filter is based on bitmap comparision.

Bitmap caching
--------------
All bitmaps are cached during an execution of completing-read. 
For special commands (execute-extended-command, describe-function,
smex etc) this cache is shared between executions.

Result caching
--------------
If previous completion is a prefix of current completion, speed hack
reuses results from previous call so ido does not start from original
(long) list.

Licence
=======

    Copyright (C) 2012  Daniel Skarda
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see [http://www.gnu.org/licenses/].

Links
=====

[ido-mode-el]: https://github.com/orfelyus/ido-mode-el
[ido-better-flex]: https://github.com/orfelyus/ido-better-flex
