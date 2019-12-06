-module(alsa_mixer).
-export([
    open/1,
    close/1,
    get_controls/1,
    set_playback_volume_all/3,
    set_playback_switch_all/3
]).

-export_type([
    mixer/0,
    control_id/0,
    control_state/0,
    control_state_playback/0
]).

-export([
    start_link/3
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    ref                   :: mixer(),
    owner_pid             :: pid(),
    owner_monitor         :: reference(),
    port                  :: port(),
    controls = []         :: [control()],
    pending_requests = [] :: [{integer(), term()}]
}).

-opaque mixer()       :: reference().
-type control()       :: {control_id(), control_state()}.
-type control_id()    :: {string(), non_neg_integer()}.
-type control_state() :: #{
    playback => control_state_playback()
}.
-type control_state_playback() :: #{
    range         => {integer(), integer()},
    db_range      => {float(), float()},
    volume        => tuple(), % tuple(integer())
    volume_db     => tuple(), % tuple(float())
    volume_joined => boolean(),
    switch        => tuple(), % tuple(on | off)
    switch_joined => boolean()
}.

-include_lib("kernel/include/logger.hrl").

-define(server(Ref), {via, reg, {alsa_mixer_reg, Ref}}).


-spec open(string() | binary()) -> {ok, mixer()} | {error, device_not_found}.
open(Device) ->
    Ref = make_ref(),
    Owner = self(),
    case alsa_mixer_sup:start_mixer(Ref, Owner, Device) of
        {ok, _Pid} ->
            {ok, Ref};
        {error, _} = Error ->
            Error
    end.


-spec close(mixer()) -> ok.
close(Mixer) ->
    gen_server:stop(?server(Mixer)).


-spec get_controls(mixer()) -> {ok, [control_id()]}.
get_controls(Mixer) ->
    gen_server:call(?server(Mixer), get_controls).


-spec set_playback_volume_all(mixer(), control_id(), Volume) -> ok | {error, Error} when
    Error :: control_not_found,
    Volume :: integer().
set_playback_volume_all(Mixer, ControlId, Value) when is_integer(Value) ->
    gen_server:call(?server(Mixer), {set_playback_volume_all, {ControlId, Value}}).


-spec set_playback_switch_all(mixer(), control_id(), Switch) -> ok | {error, Error} when
    Error :: control_not_found,
    Switch :: on | off.
set_playback_switch_all(Mixer, ControlId, State) when State =:= on; State =:= off ->
    gen_server:call(?server(Mixer), {set_playback_switch_all, {ControlId, State}}).


%% @private
-spec start_link(term(), pid(), string()) -> {ok, pid()} | {error, term()}.
start_link(Ref, Owner, Device) ->
    gen_server:start_link(?server(Ref), ?MODULE, {Ref, Owner, Device}, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @hidden
init({Ref, Owner, Device}) ->
    Port = alsa_mixer_port:open([Device]),
    init_(#state{
        ref = Ref,
        owner_pid = Owner,
        owner_monitor = erlang:monitor(process, Owner),
        port = Port,
        controls = []
    }).

init_(#state{port = Port} = State) ->
    receive
        {Port, {data, <<>>}} ->
            {ok, State};
        {Port, {exit_status, 66}} ->
            {stop, device_not_found};
        {Port, {exit_status, Code}} ->
            {stop, {internal, Code}}
    after
        1000 ->
            {stop, timeout}
    end.


%% @hidden
handle_call(get_controls, _From, State) ->
    #state{controls = Controls} = State,
    {reply, {ok, [Id || {Id, _} <- Controls]}, State};

handle_call({Command, Args}, From, State) ->
    #state{port = Port, pending_requests = PendingRequests} = State,
    ReqId = next_request_id(PendingRequests),
    port_command(Port, term_to_binary({ReqId, Command, Args})),
    {noreply, State#state{
        pending_requests = [{ReqId, From} | PendingRequests]
    }};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.


%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.


%% @hidden
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    #state{ref = Ref, owner_pid = OwnerPid, controls = Controls} = State,
    case erlang:binary_to_term(Data) of
        {add, {ControlId, ControlState0}} ->
            ControlState = control_state(ControlState0),
            notify_owner(OwnerPid, Ref, {add, ControlId, ControlState}),
            {noreply, State#state{
                controls = [{ControlId, ControlState} | Controls]
            }};
        {remove, {ControlId, ControlState0}} ->
            ControlState = control_state(ControlState0),
            notify_owner(OwnerPid, Ref, {remove, ControlId, ControlState}),
            {noreply, State#state{
                controls = lists:keydelete(ControlId, 1, Controls)
            }};
        {update, {ControlId, ControlState0}} ->
            ControlState = control_state(ControlState0),
            notify_owner(OwnerPid, Ref, {update, ControlId, ControlState}),
            {noreply, State#state{
                controls = lists:keyreplace(ControlId, 1, Controls, {ControlId, ControlState})
            }};
        {reply, ReqId, Reply} ->
            #state{pending_requests = PendingRequests} = State,
            {value, {ReqId, From}, RemainingPendingRequests} = lists:keytake(ReqId, 1, PendingRequests),
            gen_server:reply(From, Reply),
            {noreply, State#state{
                pending_requests = RemainingPendingRequests
            }};
        synchronized ->
            {noreply, State}
    end;

handle_info({'DOWN', OwnerMonitor, process, _OwnerPid, _Info}, #state{owner_monitor = OwnerMonitor} = State) ->
    {stop, normal, State};

handle_info({Port, {exit_status, ExitStatus}}, #state{port = Port} = State) ->
    {stop, {port_exit, ExitStatus}, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%====================================================================
%% helpers
%%====================================================================

-spec control_state(map()) -> control_state().
control_state(State) ->
    maps:map(fun
        (playback, Playback) ->
            maps:from_list(Playback)
    end, State).


-spec notify_owner(pid(), reference(), {add | remove | update, control_id(), control_state()}) -> ok.
notify_owner(Pid, Ref, Event) ->
    Pid ! {?MODULE, Ref, Event},
    ok.


-spec next_request_id([{integer(), term()}]) -> integer().
next_request_id(Requests) ->
    next_request_id(Requests, 0).

next_request_id([], Acc) ->
    Acc;

next_request_id([{ReqId, _} | Rest], Acc) ->
    next_request_id(Rest, max(ReqId, Acc) + 1).
