<!--
     Copyright 2014, NICTA

     This software may be distributed and modified according to the terms of
     the BSD 2-Clause license. Note that NO WARRANTY is provided.
     See "LICENSE_BSD2.txt" for details.

     @TAG(NICTA_BSD)
  -->

# Overview

This directory contains files relating to a tool that translates from Galois'
separation kernel specification to CapDL.

* docs/ &mdash; End-user documentation on how to use this tool.
* examples/ &mdash; An example of the input and output spec.
* src/ &mdash; The source code of the tool itself.
* libsk/ &mdash; A subset of the libsk library (license at libsk/LICENSE)
* libskc/ &mdash; Just the subset of libc needed by libsk

The expected workflow is for the user to specify a separation kernel
configuration using the XML input specification (described in the Galois
documentation). This specification can then be passed through either Galois'
tool to target seLinux or this tool to target seL4. The seLinux target is
intended to be used for prototyping a system, while the seL4 target is used for
deployment.
