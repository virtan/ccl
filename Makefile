# (c) 2014+ virtan virtan@virtan.com
 
# Variables

OS=$(shell uname -s | tr '[:upper:]' '[:lower:]')
BYROOT=$(shell test `id -u` -eq 0 && echo || which sudo || echo "su root -c")
GIT=$(shell which git || echo install-git-$(OS))
ERLC=$(shell which erlc || echo install-erlang-$(OS))
ERL=$(shell which erl || echo install-erlang-$(OS))
PANDOC=$(shell which pandoc || echo install-pandoc-$(OS))
REBAR=$(CURDIR)/rebar
DIALYZER_DEPS_PLT=$(CURDIR)/.deps_plt
DIALYZER_DEPS=erts kernel stdlib
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
 
# Targets

all: deps compile

deps: $(REBAR)
	$(REBAR) get-deps
	$(REBAR) compile
 
update-deps: $(REBAR)
	$(REBAR) update-deps
	$(REBAR) compile
 
compile: $(REBAR)
	$(REBAR) skip_deps=true compile

release: $(REBAR) deps compile
	$(REBAR) generate

generate: release
rel: release
 
doc: $(REBAR)
	$(REBAR) skip_deps=true doc

eunit: $(REBAR) compile
	$(REBAR) skip_deps=true eunit

test: compile eunit

$(DIALYZER_DEPS_PLT): $(ERL)
	@echo Building local plt at $(DIALYZER_DEPS_PLT)
	@echo
	dialyzer --output_plt $(DIALYZER_DEPS_PLT) --build_plt \
	   --apps $(DIALYZER_DEPS) -r deps
 
dialyzer: $(DIALYZER_DEPS_PLT) $(ERL)
	dialyzer --fullpath --plt $(DIALYZER_DEPS_PLT) -Wrace_conditions -r ./ebin
 
typer: $(ERL)
	typer --plt $(DIALYZER_DEPS_PLT) -r ./src

shell: $(REBAR) $(ERL) deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)
   
pdf: $(PANDOC)
	$(PANDOC) README.md -o README.pdf
 
clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean
 
distclean: clean
	- rm -rf $(DIALYZER_DEPS_PLT)
	- rm -rvf $(CURDIR)/deps
 
rebuild: distclean deps compile

new-project: $(REBAR)
	@echo
	@read -p "Enter the name of project: " THENAME && \
	echo && \
	$(REBAR) create-app appid=$$THENAME && \
	echo "{sub_dirs, [\"rel\"]}.\n{erl_opts, [debug_info]}.\n{deps, [\n]}.\n{cover_enabled, true}.\n{eunit_opts, [verbose]}." > rebar.config && \
	mkdir rel && \
	( cd rel && $(REBAR) create-node nodeid=$$THENAME && cd .. ) && \
	sed -i.tmp "s/^\(.*{app, $$THENAME, .*\)\]\}$$/\\1, {lib_dir, \"..\"}]}/" rel/reltool.config && \
	rm -f rel/reltool.config.tmp && \
	sed -i.tmp "s/^\(.*{rel, \"$$THENAME\", \"\)1\(\",\)$$/\\10.1\\2/" rel/reltool.config && \
	rm -f rel/reltool.config.tmp && \
	echo "deps\n.eunit\nrebar\nsubs\nebin/\n.dialyzer_plt\nrel/$$THENAME" > .gitignore


# Tools

$(REBAR): $(ERLC) $(GIT) subs
	git clone https://github.com/basho/rebar.git subs/rebar
	cd subs/rebar && ./bootstrap
	cp subs/rebar/rebar $(REBAR)
	rm -rf subs/rebar

subs:
	mkdir -p subs

install-%-darwin:
	$(BYROOT) port install $* || \
	$(BYROOT) brew install $* || \
	( echo "Please, install $*" ; false )

install-%-linux:
	$(BYROOT) apt-get install $* || \
	$(BYROOT) yum install $* || \
	( echo "Please, install $*" ; false )

install-%:
	( echo "Please, install $*" | sed "s/-[a-z0-9]*$$//" ; false )

.PHONY: all deps update-deps compile doc eunit test \
	dialyzer typer shell pdf distclean rebuild rebar \
	generate release rel new-project


