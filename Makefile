PACKAGE := erlangxl
VERSION := \
	`./version.sh`
REVISION := \
	`git --no-pager log --max-count=1 --format=format:%H`
PV := $(PACKAGE)-$(VERSION)

SUBDIRS := \
	xl_stdlib \
	xl_json \
	xl_leveldb \
	xl_yaws \
	xl_csv \
	xl_io \
	xl_net \
	persist

SUBDIRS_CLEAN = $(patsubst %, %.clean, $(SUBDIRS))

.PHONY: clean all $(SUBDIRS) \
	spec

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:	$(SUBDIRS_CLEAN)

$(SUBDIRS_CLEAN):
	$(MAKE) -C $(@:.clean=) clean

SUBST_SPECS := \
	$(PACKAGE).spec

$(SUBST_SPECS):
	sed "s,{{VERSION}},$(VERSION),g" $@.in | \
	sed "s,{{REVISION}},$(REVISION),g" > $@

spec: $(SUBST_SPECS)
