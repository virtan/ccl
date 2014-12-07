-module(ccl).
-behaviour(gen_server).

-export([
          start/0,
          start/1,
          start_link/0,
          start_link/1,
          stop/0,

          init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          code_change/3,
          terminate/2,

          activate/2,
          deactivate/2,
          weird_func/0
        ]).

-record(state, {
          conf_file,
          configured
        }).

-record(node, {
          name,
          node_name,
          connectto,
          epmd_port,
          node_port,
          epmd_tunnel_pid,
          node_tunnel_pid,
          remote_node_pid,
          cookie,
          state = use
         }).


%% Exported

start() ->
    start([]).

start(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

start_link() ->
    start_link([]).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop() ->
    gen_server:call(?MODULE, stop).

set_node_monitor(Node) ->
    gen_server:call(?MODULE, {set_node_monitor, Node}).

del_node_monitor(Node) ->
    gen_server:call(?MODULE, {del_node_monitor, Node}).


%% Internal

init(Options) ->
    application:ensure_started(exec),
    ConfFile = proplists:get_value(conf_file, Options, "conf/ccl.conf"),
    Configured = case file:consult(ConfFile) of
        {ok, Configured1} -> [#node{name = Name, node_name = node_atom(Name),
                                    connectto = ConnectTo, state = State}
                              || {Name, ConnectTo, State} <- Configured1];
        {error, _} -> []
    end,
    ?MODULE ! connection_try,
    {ok, #state{conf_file = ConfFile, configured = Configured}}.


handle_call({set_node_monitor, Node}, _From, #state{} = State) ->
    catch erlang:monitor_node(Node, true),
    {reply, ok, State};

handle_call({del_node_monitor, Node}, _From, #state{} = State) ->
    catch erlang:monitor_node(Node, false),
    receive
        {nodedown, Node} -> ignore
    after 0 -> ignore
    end,
    {reply, ok, State};

handle_call(stop, _From, #state{configured = Configured} = State) ->
    Configured1 = cleanup(Configured),
    State1 = State#state{configured = Configured1},
    {stop, normal, stopped, State1};

handle_call(Unexpected, _From, #state{} = State) ->
    {stop, {error_unexpected, Unexpected}, error_unexpected, State}.


handle_cast(command, #state{} = State) ->
    {noreply, State};

handle_cast(Unexpected, #state{} = State) ->
    {stop, {error_unexpected, Unexpected}, State}.


handle_info(connection_try, #state{configured = Configured} = State) ->
    [activate(Node) || Node <- Configured, Node#node.state == use],
    erlang:send_after(60000, ?MODULE, connection_try),
    {noreply, State};

handle_info({status_update, #node{name = NodeName} = NodeStatus},
            #state{configured = Configured} = State) ->
    Configured1 = lists:map(fun(#node{name = NodeName1}) when NodeName1 == NodeName ->
                                    NodeStatus;
                               (X) -> X
                            end, Configured),
    State1 = State#state{configured = Configured1},
    %%sync_conf(State1),
    {noreply, State1};

handle_info({nodedown, NodeAtom}, #state{configured = Configured} = State) ->
    [DownNode] = [ConfiguredNode || ConfiguredNode <- Configured,
                                    ConfiguredNode#node.node_name == NodeAtom],
    deactivate(DownNode),
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


activate(Node) ->
    spawn(?MODULE, activate, [spawned, Node]).

activate(spawned, Node) ->
    random:seed(os:timestamp()),
    put(status, Node),
    try 
        [begin
             Node1 = get(status),
             case OperationF(Node1) of
                 Node1 -> no_changes;
                 Node2 ->
                     ?MODULE ! {status_update, Node2},
                     put(status, Node2)
             end
         end ||  OperationF <- [fun cleanup/1,
                                fun set_in_progress/1,
                                fun gen_random_cookie/1,
                                fun make_epmd_tunnel/1,
                                fun run_remote_node/1,
                                fun make_node_tunnel/1,
                                fun connect_node/1,
                                fun ask_to_set_node_monitor/1,
                                fun load_modules/1,
                                fun set_on/1]]
    catch
        T:E ->
            NodeX = get(status),
            case cleanup(NodeX) of
                NodeX -> just_die;
                NodeY -> ?MODULE ! {status_update, NodeY}
            end,
            log_error("~p:~p~n~p~n", [T, E, erlang:get_stacktrace()])
    end.

deactivate(Node) ->
    spawn(?MODULE, deactivate, [spawned, Node]).

deactivate(spawned, Node) ->
    Node1 = cleanup(Node),
    ?MODULE ! {status_update, Node1},
    ?MODULE ! connection_try,
    log_error("~s disconnected~n", [Node#node.name]).

log_error(Fmt, Args) ->
    io:format(Fmt, Args).

set_in_progress(Node) ->
    Node#node{state = in_progress}.

set_on(Node) ->
    Node#node{state = on}.

gen_random_cookie(Node) ->
    Node#node{cookie = vutil:any_to_list(base64:encode(crypto:rand_bytes(60)))}.

sync_conf(#state{conf_file = ConfFile, configured = Configured}) ->
    Data = [io_lib:format("~p.~n", [{Node#node.name, Node#node.connectto,
                                     simplify_state(Node#node.state)}])
            || Node <- Configured],
    file:write_file(ConfFile, Data).

simplify_state(off) -> off;
simplify_state(_) -> use.

get_our_epmd() ->
    %% erl_epmd
    {"localhost", 4369}.

make_epmd_tunnel(#node{connectto = ConnectTo} = Node) ->
    RemPort = 4000 + random:uniform(20000),
    {OurEPMDAddr, OurEPMDPort} = get_our_epmd(),
    {ok, _Pid, OSPid} = exec:run(lists:flatten(
        ["/usr/bin/env ssh ",
         "-nNTR ", integer_to_list(RemPort), ":", OurEPMDAddr, ":",
            integer_to_list(OurEPMDPort), " ",
         ConnectTo]
        ), []),
    Node#node{epmd_port = RemPort, epmd_tunnel_pid = OSPid}.

run_remote_node(#node{name = Name,
                      connectto = ConnectTo,
                      epmd_port = EPMDPort,
                      cookie = Cookie} = Node) ->
    {ok, _Pid, OSPid} = exec:run(lists:flatten(
        ["/usr/bin/env ssh -t -t ", ConnectTo, " ",
         "ERL_EPMD_PORT=", integer_to_list(EPMDPort), " ",
            "/usr/bin/env erl ",
            "-sname ", Name, "@localhost ",
            "-setcookie ", Cookie, " ",
            "-master ", atom_to_list(node()), " ",
            "-noinput"]
        ), []),
    Node#node{remote_node_pid = OSPid}.

make_node_tunnel(#node{name = Name,
                       connectto = ConnectTo} = Node) ->
    [Port1] = vutil:run_wait(10000, 200, fun() ->
            {ok, Names} = net_adm:names(),
            case [Port || {Name1, Port} <- Names, Name1 == Name] of
                [] -> wait;
                Other -> Other
            end
        end),
    {ok, _Pid, OSPid} = exec:run(lists:flatten(
        ["/usr/bin/env ssh ",
         "-nNTL ", integer_to_list(Port1), ":localhost:",
            integer_to_list(Port1), " ",
         ConnectTo]
        ), []),
    Node#node{node_port = Port1, node_tunnel_pid = OSPid}.

node_atom(Name) ->
    list_to_atom(lists:flatten([Name, "@localhost"])).

connect_node(#node{name = Name, node_name = NodeAtom, cookie = Cookie} = Node) ->
    erlang:set_cookie(NodeAtom, list_to_atom(Cookie)),
    pong = vutil:run_wait(3000, 100, fun() ->
                case net_adm:ping(NodeAtom) of
                    pong -> pong;
                    _ -> wait
                end
                                     end),
    Node.

ask_to_set_node_monitor(#node{node_name = NodeAtom} = Node) ->
    set_node_monitor(NodeAtom),
    Node.

ask_to_del_node_monitor(#node{node_name = NodeAtom} = Node) ->
    del_node_monitor(NodeAtom),
    Node.

load_modules(#node{node_name = NodeAtom} = Node) ->
    RemoteModules = rpc:call(NodeAtom, erlang, loaded, []),
    RMSet = sets:from_list(RemoteModules),
    ModulesToSend =
        [LocalModule || LocalModule <- erlang:loaded(), not sets:is_element(LocalModule, RMSet)],
    Node.

optional_action(undefined, _) -> done;
optional_action(Defined, F) -> F(Defined).

cleanup(#node{state = off} = Node) -> Node;
cleanup(#node{epmd_tunnel_pid = EPMDPid,
              node_tunnel_pid = NodeTunnelPid,
              remote_node_pid = RemoteNodePid} = Node) ->
    ask_to_del_node_monitor(Node),
    SIGQUIT = 3,
    KillerF = fun(Pid) -> exec:kill(Pid, SIGQUIT) end,
    optional_action(RemoteNodePid, KillerF),
    optional_action(NodeTunnelPid, KillerF),
    optional_action(EPMDPid, KillerF),
    Node#node{epmd_port = undefined,
              node_port = undefined,
              epmd_tunnel_pid = undefined,
              node_tunnel_pid = undefined,
              remote_node_pid = undefined,
              cookie = undefined,
              state = use};
cleanup(Nodes) when is_list(Nodes) ->
    [cleanup(Node) || Node <- Nodes].

weird_func() ->
    sync_conf(1).
