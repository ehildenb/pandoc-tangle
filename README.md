Pandoc Tangle
=============

This library allows for tangling documents in Pandoc's internal representation.
A set of default tanglers can be accessed with the command `pandoc-tangle`.

See [the Tangle library](TangleLib.md) for an overview of the library
functionality (for developing).

### Outstanding Bug

-   Using option `--strip-text` changes what parts of the document make it
    through incorrectly.

Installing
----------

First make sure to have the tool
[Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed.
It comes in some package on most Linux distros, and is available via Homebrew on
MacOS. Then clone this repository, run `stack setup`, followed by `stack build`,
followed by `stack install`. It will copy an executable to `~/.local/bin` (or
somewhere like that), which you can either copy to somewhere on your `PATH`, or
symlink to from somewhere in your `PATH`.

Example Usage
-------------

These examples can be run in the git repository over the document `Tangle.md`,
from which the rest of the documents in the git repository are derived.

Include sections "Main Functionality" and "Predicates over Documents", but only
keep code marked with "lib" in the resulting document:

``` {.sh .example}
$ pandoc-tangle --from markdown --to markdown --code lib \
                --section 'Main Functionality|Predicates over Documents' Tangle.md
```

Same, but write to HTML instead of Markdown and to include code marked with both
"lib" and "main":

``` {.sh .example}
$ pandoc-tangle --from markdown --to html --code "lib|main" \
                --section 'Main Functionality|Predicates over Documents' Tangle.md
```

Main Functionality
------------------

The default tangler imports `Pandoc` and the library tangler to implement some
simple defaults.

Run `stack setup`, followed by `stack build` to build the executable. You can
run `stack install` to place it at `~/.local/bin`.

See `pandoc-tangle --help` for usage.

Default Readers
---------------

Only the default `readers` [from
Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `READER` specified on the command-line will be looked up in
the list of Pandoc readers.

Tanglers
--------

### Sections

The `SECTIONS` input is the name of a section to keep in the document.
Super-sections will be preserved as well. To include multiple sections separate
them with `|`, eg "Section 1|Section 4.3" would grab both "Section 1" and
"Section 4.3".

### Code

The `CODEBLOCKS` input is the class of code-blocks to keep in the document. Any
code-blocks not labelled with `CODEBLOCKS` will not be kept. To include multiple
code classes separate them with `|`, eg "main|lib" would include code classes
marked with either "main" or "lib".

Default Writers
---------------

The default `writers` [from
Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `WRITER` specified on the command-line will be looked up in
the list of Pandoc writers.

In addition, the `code-LANG` writer will produce just the code, where `LANG` is
the programming language comment-style to use for headers.
