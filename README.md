This is an unpolished keyboard-based image editor, intended for small images.
It can be run with `racket main.rkt`; the command-line flags and arguments are
described by `racket main.rkt --help`. What follows is a brief description of
how to control the editor.

Navigation
----------

The navigation keys are vim-like. One uses h, j, k, and l to move left, down,
up, and right, respectively. Holding down shift causes movement by 5 instead of
1, and ctrl causes movement by 10. 0, $, (, and ) will move to the far left,
right, upper, and lower borders, respectively. You can toggly cursor visibility
with c. You can zoom in/out with - and +.

Colours
-------

The right portion of the screen displays the RGBA colour of your brush. You can
increase each component with r, g, b, and a; you can decrease each component
with e, d, v, and z. Using shift changes a colour by 10, and using ctrl changes
a colour by 50. You can also load the colour of the pixel under the cursor with
q. You have 9 different brushes; you can switch between them with 1-9. Press d
to apply the colour to the current pixel. You can press u to undo your last change.

Saving/Loading
--------------

You can save your file by pressing s. The file will be saved to the filename
you passed as an argument.  There is no indication that the file has been
saved. You can load a file simply (and only) by passing its name as the initial
command-line argument. The file will be overwritten if you save.
