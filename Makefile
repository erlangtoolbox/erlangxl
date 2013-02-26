PROJECT=erlangxl
VERSION=`cat version`
RELEASE=`cat release`
REVISION=`git --no-pager log --max-count=1 --format=format:%H`

.PHONY: all compile install doc doc-install eunit clean dialyze all-tests spec

APPS =	xl_stdlib \
	xl_json \
	xl_leveldb \
	xl_yaws \
	xl_csv \
	xl_io \
	xl_net \
	persist

.PHONY: $(APPS)

all: $(APPS)

compile: $(APPS)

install: $(APPS)

doc: $(APPS)

doc-install: $(APPS)

eunit: $(APPS)

all-tests:
	$(MAKE) TEST=yes clean compile
	$(MAKE) eunit
## do separate build because dialyzer produces a lot
## of warnings for eunit code
	$(MAKE) DEBUG=yes clean compile
	$(MAKE) dialyze

clean: $(APPS)
	rm --force -- $(PROJECT).spec

$(APPS):
	$(MAKE) -C "$@" $(MAKECMDGOALS)

spec: opensuse.spec.in
	cat $< | \
		sed "s/{{VERSION}}/$(VERSION)/" | \
		sed "s/{{RELEASE}}/$(RELEASE)/" | \
		sed "s/{{REVISION}}/$(REVISION)/" \
		> $(PROJECT).spec

PLT=.dialyzer_plt

dialyze: $(PLT)
	dialyzer --plt $< -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions
	dialyzer --src --plt $< -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions

$(PLT):
	dialyzer --build_plt --output_plt $@ \
		--apps erts kernel stdlib crypto compiler

