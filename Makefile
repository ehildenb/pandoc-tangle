.PHONY: css-test tangle-test test

test: css-test tangle-test

css-test:
	lua test/css-selector.lua

tangle-test:
	./test/tangle test/doc1.md test/doc1-ANY.out
	./test/tangle test/doc1.md test/doc1-ANY.out           '*'
	./test/tangle test/doc1.md test/doc1-sh.out            '.sh'
	./test/tangle test/doc1.md test/doc1-one.out           '.one'
	./test/tangle test/doc1.md test/doc1-sh-one.out        '.sh.one'
	./test/tangle test/doc1.md test/doc1-sh-one.out        '.one.sh'
	./test/tangle test/doc1.md test/doc1-not-one.out       ':not(.one)'
	./test/tangle test/doc1.md test/doc1-sh-or-not-one.out '.sh,:not(.one)'
