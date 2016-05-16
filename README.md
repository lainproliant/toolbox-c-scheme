Lain's Toolbox for Chicken Scheme
=================================

This toolbox provides useful general-purpose functions and macros for the Chicken Scheme programming environment.

This library will grow as my experience with practical Scheme programming progresses.

See the 'Index' section below for a list of all functions and macros provided and a brief explanation.  Please refer to the documentation in the source for further reference.

Usage
-----

To use the toolbox in your projects, copy or add it as a submodule to your project, and add it to your include path.

```sh
$ git submodule add https://github.com/lainproliant/toolbox-c-scheme
```

In your source files, include `toolbox-scheme.scm`.  This will include all of the source files in the toolbox.

```scheme
(include "toolbox-scheme.scm")
```

From there, you can import the modules you wish to use.  The list of available modules corresponds to the source files in the root of the toolbox, minus `toolbox-scheme.scm`.

```scheme
(import (prefix toolbox-util toolbox:))
```
Each module name is prefixed with `toolbox-` to avoid conflicting with other modules.  Since namespacing is a good idea, I recommend using the prefix form whenever you are importing modules from the toolbox.

Index
=====

toolbox-argv
------------
This module contains tools that are useful for dealing with parameters and building command line interfaces.

### subcom-add!
```
(subcom-add! sc name fun)
```
Adds or redefines a subcommand in the subcommand map `sc`.

### subcom-invoke
```
(subcom-invoke sc [argv])
```
Run a subcommand based on the arguments in argv, uses the current
command line argument list by default.

### subcom-create
```
(subcom-create [subcom-specs])
```
Creates and optional initializes a subcommand map with the given
list of command name/1-ary lambda tuples.

### subcom-default!
```
(subcom-default! sc default)
```
Specify the default command.

### subcommands
```
(subcommands ((name func) ...))
```
Create a subcommand map with the given list of name/1-ary lambda tuples.
This is the macro form of `subcom-create` and is the preferred way to
create subcommand maps.

toolbox-test
------------
This module offers a very simple unit testing framework.

### \*exn-rethrow\*
Determines if exceptions thrown by unit tests are passed up to
the caller of `test-suite`.  False by default, can be overridden
by setting the environment variable `TOOLBOX_EXN_RETHROW` to `1`.

### test-suite
```
(test-suite name . tests)
```
Create and immediately execute a test suite.  Each test function is
executed in succession.  The results of the test suite are printed
to standard-out.

util
----
This module provides general purpose utilities and shortcuts for
commonly used simple function combinations.

### empty?
```
(empty? lst)
```
Returns true if the list is empty, false otherwise.

### first
```
(first lst)
```
A synonym for `car`.

### first-or
```
(first-or lst default)
```
Returns the first element of `lst` or `default` if `lst` is empty.

### getenv
```
(getenv name)
```
Gets the OS environment variable specified by `name` or `#f` if the
variable is not set.

