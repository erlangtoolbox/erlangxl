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


# check-deps:
# 	@for APP in $(DEPS); do \
# 		VER=`$(VERSION_ERLVAL)`; \
# 		[ $$? == 0 ] || { echo "Required app $$APP not installed"; exit 1; }; \
# 		echo "App $$APP version $$VER"; \
# 	done

# yo rebar!
eunit: compile
	rebar eunit


version:
#	@LASTTAG=`git tag -l | tail -n1`; \
#	[ x$$LASTTAG == x ] && LASTTAG="0.0.0"; \
#	echo "$$LASTTAG";
#	echo "\"$${LASTTAG}_$$LASTCOMMITTIME\"" >VERSION
##	just use serial
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
#	find .dist/$$DISTDIR -name \*.app -exec sed -i 's,@VSN@,"$(VERSION)",' '{}' \;
## The idea being to push out the common (package) version from
## VERSION to all individual applications, to enable
## $(VERSION_ERLVAL) to retrieve it from any one of them.
## Still unsure this is the way to go.


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

