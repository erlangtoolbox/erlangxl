PACKAGE = strikead-erlang-commons
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

DEPS = \
	erlando \
	ktuo \
	yaws \
	flake \
	erleveldb

.PHONY: subdirs $(SUBDIRS) \
	all clean compile \
	spec rpm

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	@find . -name \*.beam -delete; \
	rm -rf .dist

# yo rebar!
eunit: compile
	rebar eunit


version:
	@LASTCOMMITTIME=`git log -1 --pretty=format:"%ci" | sed 's/[\ :-]//g' | sed 's/\+[0-9]\{4\}//'`; \
	echo "$$LASTCOMMITTIME" >VERSION

distdir: version
	@VERSION=`cat VERSION`; \
	PV=$(PACKAGE)-$$VERSION; \
	DISTDIR=$(SOURCES)/$$PV; \
	rm -rf $$DISTDIR; \
	mkdir -p $$DISTDIR && \
	find $(SUBDIRS) -type f \
		-not -name \*.beam \
		-not -name .backups \
		-not -name \*~ \
		-not -name TAGS \
		-exec cp --parents -t $$DISTDIR '{}' \; ; \
	for F in Makefile Makefile.inc VERSION; do cp $$F $$DISTDIR; done


dist-bzip2: distdir
	@VERSION=`cat VERSION`; \
	PV=$(PACKAGE)-$$VERSION; \
	DISTDIR=$(SOURCES)/$$PV; \
	(cd $(SOURCES) && tar -cjf $$PV.tar.bz2 $$PV); \
	rm -rf $$DISTDIR; \
	echo "Created $$PV.tar.bz2"


dist-gzip: distdir
	@VERSION=`cat VERSION`; \
	PV=$(PACKAGE)-$$VERSION; \
	DISTDIR=$(SOURCES)/$$PV; \
	(cd $(SOURCES) && tar -czf $$PV.tar.gz $$PV); \
	rm -rf $$DISTDIR; \
	echo "Created $$PV.tar.gz"

spec: version
#	determine current versions of all deps and
	@VERSION=`cat VERSION`; \
	PV=$(PACKAGE)-$$VERSION; \
	sed "s,{{VERSION}},$$VERSION," \
		$(PACKAGE).spec.in > $(SPECS)/$$PV.spec; \
	echo "Generated $$PV.spec"

rpm: dist-gzip spec
	@VERSION=`cat VERSION`; \
	PV=$(PACKAGE)-$$VERSION; \
	rpmbuild -ba $(SPECS)/$$PV.spec

