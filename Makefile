SCHEME_INTERPRETER=csi -I .. -q -s
CHICKEN_INSTALL=sudo chicken-install
DEP_EGGS=\
	args \
	posix-extras

all: build-all-tests clean

deps:
	$(foreach egg,$(DEP_EGGS),$(CHICKEN_INSTALL) $(egg);)

build-all-tests: $(patsubst test/%.scm, test/%.output, $(wildcard test/*.scm))

test/%.output: test/%.scm Makefile
	$(SCHEME_INTERPRETER) $(SCHEME_INTERPRETER_FLAGS) $<

clean:
	rm -f *.output

