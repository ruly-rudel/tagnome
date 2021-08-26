STACK = stack
TARGET = tagnome


.PHONY:	build run test config clean distclean

build:
	$(STACK) build

run:
	$(STACK) run

test:
	$(STACK) test

test-debug:
	$(STACK) test --ghc-options="-g" --no-strip

config:	build hie.yaml stack-hls.yaml


clean:
	$(STACK) purge; rm -rf *.lock $(TARGET).cabal

distclean:	clean
	rm -rf hie.yaml stack-hls.yaml

hie.yaml:	package.yaml
	gen-hie | sed '2a\    stackYaml: "stack-hls.yaml"\n    components:' > hie.yaml

stack-hls.yaml:	stack.yaml
	(cat stack.yaml && echo -e 'ghc-options:\n  "$locals": -O0 -Wall -haddock\n  "$everything": -haddock') > stack-hls.yaml
