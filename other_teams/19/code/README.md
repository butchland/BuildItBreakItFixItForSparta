error309-bibifi
===============

Built-it-Break-it-Fix-it

The Makefile
============

The makefile has two intended uses for running the program.
* standard running the program
      clean > configure > build > run     
* running the program with RTS (run-time-system) enabled.  This is helpful if
  you want to kill a program and look at its stack trace, like if you're
  debugging an infinite loop.
      clean > configure-RTS > build > run-RTS


