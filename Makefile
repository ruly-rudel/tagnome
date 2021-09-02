STACK = stack
TARGET = tagnome
TMP = ./tmp


.PHONY:	build run test config clean distclean

build:
	$(STACK) build

run:
	$(STACK) run

test:
	rm -rf $(TMP)/*
	$(STACK) test
	diff ./tv/01/tone1.flac ./tmp/tone1copy.flac

test-debug:
	$(STACK) test --ghc-options="-g" --no-strip

config:	build hie.yaml stack-hls.yaml


clean:
	$(STACK) purge; rm -rf *.lock $(TARGET).cabal $(TMP)/*

distclean:	clean
	rm -rf hie.yaml stack-hls.yaml

hie.yaml:	package.yaml
	gen-hie | sed '2a\    stackYaml: "stack-hls.yaml"\n    components:' > hie.yaml

stack-hls.yaml:	stack.yaml
	(cat stack.yaml && echo -e 'ghc-options:\n  "$locals": -O0 -Wall -haddock\n  "$everything": -haddock') > stack-hls.yaml
