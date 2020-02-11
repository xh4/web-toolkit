# find-port

[![Build Status](https://travis-ci.org/eudoxia0/find-port.svg?branch=master)](https://travis-ci.org/eudoxia0/find-port)
[![Quicklisp](http://quickdocs.org/badge/find-port.svg)](http://quickdocs.org/find-port/)

Find open ports programmatically.

# Overview

```lisp
CL-USER> (find-port:port-open-p 5000)
T
CL-USER> (my-clack-app:start)
To load "clack-handler-hunchentoot":
  Load 1 ASDF system:
    clack-handler-hunchentoot
; Loading "clack-handler-hunchentoot"

Hunchentoot server is started.
Listening on localhost:5000.
T
CL-USER> (find-port:port-open-p 5000)
NIL
CL-USER> (find-port:find-port)
50123
```

# Usage

The `port-open-p` function takes an integer and determines whether a port by
that number is open.

The `find-port` function, by default takes no arguments, and returns an open
port. Two keyword arguments, `:min` and `:max`, may be given to constrain the
port range in which to search for ports.

By default, the range is from 40000 to 50000, since this function will probably
be used e.g. to pick an open port for testing, rather than to pick a port to run
a production server on.

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
