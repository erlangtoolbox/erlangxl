# ; -*- mode: makefile -*-

ERLS  = $(wildcard src/*.erl)
HRLS  = $(wildcard include/*.hrl)
BEAMS = $(patsubst src/%.erl, ebin/%.beam, $(ERLS))

ERL_FLAGS = \
	+warn_unused_function \
	+warn_bif_clash \
	+warn_deprecated_function \
	+warn_obsolete_guard \
	+warn_shadow_vars \
	+warn_export_vars \
	+warn_unused_records \
	+warn_unused_import


.PHONY: compile clean all

compile: $(BEAMS) $(EXTRA_TARGETS)

ebin/%.beam: src/%.erl $(HRLS)
	-mkdir -p ebin
	erlc -o ./ebin -I ./include $(ERL_FLAGS) \
		$(EXTRA_ERLFALGS) \
		$<
clean:
	-rm -f $(BEAMS)

