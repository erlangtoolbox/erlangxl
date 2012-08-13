BINDS = $(wildcard */*.bind)
EXTRA_TARGETS = $(BINDS)
%.bind:
	erl -pz ../strikead_json/ebin -pz ../strikead_stdlib/ebin \
		-noshell \
		-run strikead_json_binder compile $(@D) $< -s init stop

include ../Makefile.inc
