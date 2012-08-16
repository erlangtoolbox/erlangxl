PACKAGE = strikead-erlang-commons
PROJECT = erlang-commons
VERSION = \
	`git log -1 --pretty=format:"%ci" | sed 's/[\ :-]//g' | sed 's/\+[0-9]\{4\}//'`
PV = $(PACKAGE)-$(VERSION)

SPECS = $(DESTDIR)/SPECS
SOURCES = $(DESTDIR)/SOURCES

SUBDIRS = \
	strikead_stdlib \
	strikead_json \
	strikead_leveldb \
	strikead_yaws \
	strikead_csv \
	strikead_eunit \
	strikead_io \
	strikead_net

SUBDIRS_CLEAN = $(patsubst %, %.clean, $(SUBDIRS))

.PHONY: clean all $(SUBDIRS) \
	rpm

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:	$(SUBDIRS_CLEAN)

$(SUBDIRS_CLEAN):
	$(MAKE) -C $(@:.clean=) clean

rpm: clean
	@tar czf $(SOURCES)/$(PV).tar.gz ../$(PROJECT)
	sed "s,{{VERSION}},$(VERSION)," \
		$(PACKAGE).spec.in > $(SPECS)/$(PV).spec
	rpmbuild -ba $(SPECS)/$(PV).spec
