BINDS = $(wildcard */*.bind)
EXTRA_TARGETS = $(BINDS)
%.bind:
	erl -pz ../xl_json/ebin -pz ../xl_stdlib/ebin \
		-noshell \
		-run xl_json_binder compile $(@D) $< -s init stop

include ../Makefile.inc
