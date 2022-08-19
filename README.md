Pandoc Tangle
=============

Pandoc Tangle is an output target for [Pandoc] which allows writing selected codeblocks into files.
This allows for semi-literate programming, allowing for code to be self-presenting while respecting any issues in tooling for the respective languages.

Dependencies
------------

-   [Pandoc] - universal converter between document formats.
-   [Lua] - scripting language used for [Pandoc] filters.

**NOTE**: You must `export LUA_PATH=/path/to/pandoc-tangle/?.lua;;` before running `pandoc` so that the Lua `require ...` statements work.

Example Usage
-------------

The examples here are given for the Markdown format (though in principle any format witch preserves the CSS-class codeblock attributes should work).
See more examples in the `Makefile` and `test/` directory.

1.  Add tags to each codeblock describing their usage:

    ``````
    ```{.tag1 .sh}
    codeblock1
    ```

    ```tag1
    codeblock2
    ```

    ```
    codeblock3
    ```

    ```tag3
    codeblock4
    ```
    ``````

2.  Use `pandoc --to tangle.lua --metadata=code:CSS-SELECTOR` to select the desired codeblocks using [CSS selectors]:

    ```sh
    # selects both codeblock1 and codeblock2
    $ pandoc --from markdown --to tangle.lua --metadata=code:.tag1

    # selects only codeblock1
    $ pandoc --from markdown --to tangle.lua --metadata=code:.tag1.sh

    # selects only codeblock1
    $ pandoc --from markdown --to tangle.lua --metadata=code:.tag1.sh

    # selects codeblock3 and codeblock4
    $ pandoc --from markdown --to tangle.lua --metadata=code::not(.tag1)

    # selects codeblock2
    $ pandoc --from markdown --to tangle.lua --metadata=code:.tag1:not(.sh)

    # selects codeblock1 and codeblock4
    $ pandoc --from markdown --to tangle.lua --metadata=code:.sh,.tag3
    ```

Supported [CSS selectors] grammar
---------------------------------

Currently, a boolean algebra with top (or bottom) is supported.
Note that AND (justaposition) binds tighter than OR (comma).

```
    syntax CSS-ID ::= regex"[%a%d-]+"                     // alphanumerics and dashes

    syntax CSS-SELECTOR ::= "." CSS-ID                    // raw identifier
                          | ":not(" CSS-SELECTOR ")"      // negation
                          | CSS-SELECTOR CSS-SELECTOR     // juxtaposition is AND
                          > CSS-SELECTOR "," CSS-SELECTOR // comma is OR
```

Resources
---------

- [Pandoc](https://pandoc.org)
- [Lua](https://www.lua.org)
- [CSS selectors](https://www.w3schools.com/cssref/css_selectors.asp)
- [Universal selector in CSS](https://www.scaler.com/topics/universal-selector-in-css/)
