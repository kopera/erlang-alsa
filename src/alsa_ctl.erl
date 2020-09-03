-module(alsa_ctl).
-export([
    open/1,
    close/1
]).
-export([
    elem_list/1,
    elem_info/2,
    elem_read/2,
    elem_write/3
]).
-export_type([
    ctl/0
]).

-on_load(init/0).


%% @doc Opens a CTL. The returned CTL handle will be closed automatically if the
%% opening process terminates.
%%
%% @param Name      ASCII identifier of the CTL handle
%% @returns a handle to the CTL device on success otherwise an error.
-spec open(string() | binary()) -> {ok, ctl()} | {error, Error}
    when Error :: enomem | pos_integer().
-opaque ctl() :: reference().
open(Name) ->
    open_nif(unicode:characters_to_list(Name, latin1)).

%% nif
open_nif(_Device) ->
    erlang:nif_error(not_loaded).


%% @doc Closes a previously opened CTL handle.
%% @param CTL       the CTL handle to close
%% @returns ok on success otherwise an error.
-spec close(ctl()) -> ok | {error, Error}
    when Error :: closed | pos_integer().
close(CTL) ->
    close_nif(CTL).

%% nif
close_nif(_CTL) ->
    erlang:nif_error(not_loaded).


%% @doc Get a CTL element value.
%% @param CTL   the CTL handle
%% @param Id    the element identifier
-spec elem_list(ctl()) -> {ok, [elem_id()]} | {error, Error}
    when Error :: pos_integer().
-type elem_id() :: #{
    numid := integer(),
    interface := card | mixer | pcm | rawmidi | timer | sequencer,
    name := string(),
    index := non_neg_integer(),
    device := non_neg_integer(),
    subdevice := non_neg_integer()
}.
elem_list(CTL) ->
    elem_list_nif(CTL).

%% nif
elem_list_nif(_CTL) ->
    erlang:nif_error(not_loaded).


%% @doc Get a CTL element info.
%% @param CTL   the CTL handle
%% @param Id    the element identifier
-spec elem_info(ctl(), elem_id_matcher()) -> {ok, elem_info()} | {error, Error}
    when Error :: pos_integer() | enotsup.
-type elem_info() ::
      #{type := boolean, count := non_neg_integer()}
    | #{type := integer, count := non_neg_integer(), min := integer(), max := integer()}
    | #{type := enumerated, count := non_neg_integer(), items := [string()]}
    | #{type := bytes, count := non_neg_integer()}.
elem_info(CTL, Id) ->
    elem_info_nif(CTL, Id).

%% nif
elem_info_nif(_CTL, _Id) ->
    erlang:nif_error(not_loaded).


%% @doc Get a CTL element value.
%% @param CTL   the CTL handle
%% @param Id    the element identifier
-spec elem_read(ctl(), elem_id_matcher()) -> {ok, elem_value()} | {error, Error}
    when Error :: pos_integer() | enotsup.
-type elem_id_matcher() :: #{numid := integer()} | #{name := string()}.
-type elem_value() :: binary() | tuple().
elem_read(CTL, Id) ->
    elem_read_nif(CTL, Id).

%% nif
elem_read_nif(_CTL, _Id) ->
    erlang:nif_error(not_loaded).


%% @doc Set CTL element value.
%% @param CTL   the CTL handle
%% @param Id    the element identifier
%% @param Value the new element value
-spec elem_write(ctl(), elem_id_matcher(), elem_value()) -> ok | {error, Error}
    when Error :: pos_integer() | enotsup.
elem_write(CTL, Id, Value) ->
    elem_write_nif(CTL, Id, Value).

%% nif
elem_write_nif(_CTL, _Id, _Value) ->
    erlang:nif_error(not_loaded).


%
% Initialization
%

init() ->
    ok = erlang:load_nif(nif_path(), 0).

-spec nif_path() -> string() | binary() | undefined.
nif_path() ->
    Priv = case code:priv_dir(alsa) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                File when is_list(File) ->
                    filename:join([filename:dirname(File), "../priv"]);
                _ ->
                    "../priv"
            end;
        Dir ->
            Dir
    end,
    nif_path(os:type(), Priv).


nif_path(_, Dir) ->
    filename:join([Dir, "alsa_ctl_nif"]).
