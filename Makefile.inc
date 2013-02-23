# ; -*- mode: makefile -*-
APP?=$(shell basename `pwd`)
VERSION=`cat ../version`

.PHONY: all compile install doc doc-install eunit clean Emakefile figures

all: compile doc

compile: ebin/$(APP).app Emakefile
	erl -noinput -eval 'up_to_date = make:all()' -s erlang halt

install:
	install --mode=0755 --directory \
		$(DESTDIR)/$(APP)-$(VERSION)/ebin \
		$(DESTDIR)/$(APP)-$(VERSION)/include
	install --mode=644 ebin/*.app ebin/*.beam \
		$(DESTDIR)/$(APP)-$(VERSION)/ebin
	install --mode=644 include/*.hrl \
		$(DESTDIR)/$(APP)-$(VERSION)/include

doc: doc/overview.edoc figures
	erl -noinput -eval \
		'edoc:application($(APP), ".", [{application, $(APP)}])' \
		-s erlang halt

FIGSSRC=$(wildcard doc/*.dot)
FIGS=$(patsubst doc/%.dot, doc/%.png, $(FIGSSRC))

figures: $(FIGS)

doc/%.png: doc/%.dot
	dot -T png $< > $@

doc-install: doc
	mkdir --parents $(DESTDIR)/$(APP)
	install --mode=644 doc/*.html doc/*.css doc/*.png $(DESTDIR)/$(APP)

eunit:
	erl -noinput -pa ebin \
		-eval 'ok = eunit:test({application, $(APP)}, [verbose])' \
		-s erlang halt

clean:
	rm --force -- doc/*.html doc/*.css doc/*.png doc/edoc-info \
		erl_crash.dump Emakefile doc/overview.edoc
	rm --force --recursive -- ebin

ebin/$(APP).app: src/$(APP).app.src ../version
	mkdir --parents ebin
	sed "s/{{VERSION}}/$(VERSION)/" $< > $@

doc/overview.edoc: doc/overview.edoc.src ../version
	sed "s/{{VERSION}}/$(VERSION)/" $< > $@

ifdef DEBUG
EXTRA_OPTS:=$(EXTRA_OPTS) debug_info,
endif

ifdef TEST
EXTRA_OPTS:=$(EXTRA_OPTS) {d,'TEST', true},
endif

Emakefile: ../Emakefile.src
	sed "s/{{EXTRA_OPTS}}/$(EXTRA_OPTS)/" $< > $@

