%% @private
-module(alsa_sup).
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================


init([]) ->
    Flags = #{strategy => rest_for_one, intensity => 1, period => 1},
    Children = [
        reg:child_spec(alsa_mixer_reg, #{
            kind => unique
        }),
        #{
            id => alsa_mixer_sup,
            type => supervisor,
            start => {alsa_mixer_sup, start_link, []},
            restart => permanent
        }
    ],
    {ok, {Flags, Children}}.
