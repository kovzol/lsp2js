lsp2js
======

A Lisp to Javascript converter, supporting command line compilations
under Unix/Linux.

There are various Lisp to Javascripts converters out there, see
http://ceaude.twoticketsplease.de/js-lisps.html for a great comparison.

This project wants to concentrate on converting existing Lisp
projects to Javascript. At the moment, a command line tool is
provided which uses Dmitry Nizhegorodov's converter.

Prerequisites
-------------

node.js is required to run the Javascript files in command line.

Usage
-----

Change your working directory to the project root dir and copy
your .lsp file there (e.g. "example.lsp"). Then run

`./lsp2js example.jsp`

Now the file `example.js` should be created.

Bugs
----

Current version supports the same Lisp constructions as Dmitry's
original converter does.
