# ljr-post

ljr posting script. See template.ljr for the format.

Supported options: @User, @Subject, @Privacy, @Mood, @Music

usage: ljr-post -t posttext.ljr [-p] [-s pwd]

       -p

           writes preview to /tmp/preview.html and attempts to xdg-open it.

       -s

           if passed, will not ask for password, but will read it from the argument.
           Useful for additional scripting.

Installation: 

e.g. stack build, stack install
