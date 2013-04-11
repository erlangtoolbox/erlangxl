# ; -*- mode: makefile -*-
APP?=$(shell basename `pwd`)
VERSION=`cat ../version`

.PHONY: all compile install doc doc-install eunit clean Emakefile figures

all: compile doc

BINDS=$(wildcard src/*.bind)
BINDERLS=$(patsubst src/%.bind, src/%.erl, $(BINDS))
BINDHRLS=$(patsubst src/%.bind, include/%.hrl, $(BINDS))

YRLS=$(wildcard src/*.yrl)
YRLERLS=$(patsubst src/%.yrl, src/%.erl, $(YRLS))

XRLS=$(wildcard src/*.xrl)
XRLERLS=$(patsubst src/%.xrl, src/%.erl, $(XRLS))

compile: ebin/$(APP).app Emakefile $(BINDERLS) $(BINDHRLS)
	erlc -o src $(YRLS) $(XRLS)
	erl -noinput -eval 'up_to_date=make:all(),halt()'

include/%.hrl src/%.erl: src/%.bind
	erl -noinput -eval 'ok=xl_json_bindc:compile("$<","."),halt()'

install:
	install --mode=0755 --directory $(DESTDIR)/$(APP)-$(VERSION)/ebin
	install --mode=644 ebin/*.app ebin/*.beam \
		$(DESTDIR)/$(APP)-$(VERSION)/ebin
	if [ -d include ]; then \
	    install --mode=0755 --directory \
		$(DESTDIR)/$(APP)-$(VERSION)/include; \
	    install --mode=644 include/*.hrl \
		$(DESTDIR)/$(APP)-$(VERSION)/include; \
	fi

doc: doc/overview.edoc figures
	erl -noinput -eval \
		'edoc:application($(APP),".",[{application,$(APP)}]),halt()'

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
		-eval 'ok=eunit:test({application,$(APP)},[verbose]),halt()'

clean:
	rm --force -- doc/*.html doc/*.css doc/*.png doc/edoc-info \
		erl_crash.dump Emakefile doc/overview.edoc
	rm --force --recursive -- ebin $(BINDERLS) $(BINDHRLS) $(YRLERLS) $(XRLERLS)

ebin/$(APP).app: src/$(APP).app.in ../version
	mkdir --parents ebin
	sed "s/{{VERSION}}/$(VERSION)/" $< > $@

doc/overview.edoc: doc/overview.edoc.in ../version
	sed "s/{{VERSION}}/$(VERSION)/" $< > $@

ifdef DEBUG
EXTRA_OPTS:=$(EXTRA_OPTS) debug_info,
endif

ifdef TEST
EXTRA_OPTS:=$(EXTRA_OPTS) {d,'TEST', true},
endif

Emakefile: ../Emakefile.in
	sed "s/{{EXTRA_OPTS}}/$(EXTRA_OPTS)/" $< > $@

