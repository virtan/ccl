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

test: deps compile eunit

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
	- rm -f $(CURDIR)/rebar
 
rebuild: distclean deps compile

export TMPL_REBAR_CONFIG
export TMPL_GITIGNORE
new-project: $(REBAR)
	@echo
	@read -p "Enter the name of project: " THENAME && \
	echo && \
	$(REBAR) create-app appid=$$THENAME && \
	echo "$$TMPL_REBAR_CONFIG" | sed "s/PROJECTNAME/$$THENAME/g" > rebar.config && \
	mkdir rel && \
	( cd rel && $(REBAR) create-node nodeid=$$THENAME && cd .. ) && \
	sed -i.tmp "s/^\(.*{app, $$THENAME, .*\)\]\}$$/\\1, {lib_dir, \"..\"}]}/" rel/reltool.config && \
	rm -f rel/reltool.config.tmp && \
	sed -i.tmp "s/^\(.*{lib_dirs, \[\)\(\]\},\)$$/\\1"../deps"\\2/" rel/reltool.config && \
	rm -f rel/reltool.config.tmp && \
	sed -i.tmp "s/^\(.*{rel, \"$$THENAME\", \"\)1\(\",\)$$/\\10.1\\2/" rel/reltool.config && \
	rm -f rel/reltool.config.tmp && \
	echo "$$TMPL_GITIGNORE" | sed "s/PROJECTNAME/$$THENAME/g" > .gitignore

export TMPL_GEN_SERVER
gen-server:
	@echo
	@read -p "Enter the name of gen_server module: " GENSERVER && \
	echo && \
	echo "$$TMPL_GEN_SERVER" | sed "s/GENSERVERNAME/$$GENSERVER/g" > $$GENSERVER.erl && \
	( mv $$GENSERVER.erl src/ 2>/dev/null || true )


# Tools

$(REBAR): $(ERLC) $(GIT)
	mkdir subs || true
	rm -rf subs/rebar || true
	git clone https://github.com/basho/rebar.git subs/rebar
	cd subs/rebar && ./bootstrap
	cp subs/rebar/rebar $(REBAR)
	rm -rf subs/rebar

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


# System

.PHONY: all deps update-deps compile doc eunit test \
	dialyzer typer shell pdf distclean rebuild rebar \
	generate release rel new-project gen-server


# Templates

define TMPL_REBAR_CONFIG
{sub_dirs, ["rel"]}.
{erl_opts, [debug_info]}.
{deps, [
        {vutil, ".*", {git, "git://github.com/virtan/vutil.git", ""}},
	{eper, ".*", {git, "git://github.com/massemanet/eper.git", ""}}
]}.
{cover_enabled, true}.
{eunit_opts, [verbose]}.
endef

define TMPL_GITIGNORE
deps
.eunit
subs
ebin/
.dialyzer_plt
rel/PROJECTNAME
endef

define TMPL_GEN_SERVER
-module(GENSERVERNAME).
-behaviour(gen_server).

-export([
          start/1,
          start_link/1,
          stop/0,

          command/0,

          init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          code_change/3,
          terminate/2
        ]).

-record(state, {
          %% TODO: define internal state
        }).


%% Exported

start(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop() ->
    gen_server:call(?MODULE, stop).

command() ->
    gen_server:call(?MODULE, command).


%% Internal

init(_Options) ->
    %% TODO: process Options, prepare state
    {ok, #state{}}.


handle_call(command, _From, #state{} = State) ->
    {reply, the_reply, State};

handle_call(stop, _From, #state{} = State) ->
    {stop, normal, stopped, State};

handle_call(Unexpected, _From, #state{} = State) ->
    {stop, {error_unexpected, Unexpected}, error_unexpected, State}.


handle_cast(command, #state{} = State) ->
    {noreply, State};

handle_cast(Unexpected, #state{} = State) ->
    {stop, {error_unexpected, Unexpected}, State}.


handle_info(command, #state{} = State) ->
    {noreply, State};

handle_info(Unexpected, #state{} = State) ->
    {stop, {error_unexpected, Unexpected}, State}.


code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.


terminate({error_unexpected, _Unexpected}, #state{} = _State) ->
    %% TODO: report about unexpected
    ok;

terminate(_Reason, #state{} = _State) ->
    ok.

endef
