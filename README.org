* License

This code is made publicly available by SIFT, LLC under the terms of the
3-clause BSD license, attached as [[file:license.txt][license.txt]].

* Component libraries
** PDDL

This does not provide any PDDL-wrangling functions (well, with the exception of
=PPRINT-PDDL=, whose job it is to print PDDL /readably/).  It simply provides
the =PDDL= package, which is a package into which other code can read PDDL
expressions.  Having a canonical package for PDDL means that multiple different
bits of code can all wrangle PDDL without getting into package hell.  In
particular, it means that one bit of code can read PDDL into an s-expression
that other code can interact with.

** PDDL-UTILS

PDDL-wrangling functions.  These are made to allow you to read and write PDDL
files, especially problem files, domain files, and plans.  There are also
problem and domain /constructors/, /accessors/, and /modifiers/.  A key purpose
of this library is for programmers to be able to make derivative or templated
domains and problems.  You can read a problem (domain), set some of its slots
programmatically, and then write it out to a file.

** PDDL-PLANNERS

This attempts to provide a uniform-ish API to invoke a number of different
planners from Common Lisp.  This is just the core framework here.

Given a particular planning program that one wants to invoke, one extends the
set of planning methods (often adding new keyword arguments) provided here with
methods for your new planners.

This approach is only somewhat successful, since we find that planners often
have very complex invocation procedures with very particular sets of
configuration options.



* COMMENT local variables block
# Local Variables:
# mode: org
# End:
