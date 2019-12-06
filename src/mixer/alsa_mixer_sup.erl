%% @private
-module(alsa_mixer_sup).
-export([
    start_mixer/3
]).
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).


-spec start_mixer(term(), pid(), string() | binary()) -> {ok, pid()} | {error, term()}.
start_mixer(Ref, Owner, Device) when is_binary(Device) ->
    start_mixer(Ref, Owner, unicode:characters_to_list(Device));

start_mixer(Ref, Owner, Device) when is_list(Device) ->
    supervisor:start_child(?MODULE, [Ref, Owner, Device]).


-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================


init([]) ->
    Flags = #{strategy => simple_one_for_one, intensity => 1, period => 1},
    Children = [
        #{
            id => alsa_mixer,
            start => {alsa_mixer, start_link, []},
            restart => transient,
            type => worker
        }
    ],
    {ok, {Flags, Children}}.
