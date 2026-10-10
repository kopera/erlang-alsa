-module(alsa_ctl).
-moduledoc """
Access to the controls of an ALSA sound card.

The controls of a sound card, such as the volume or the switch of an output,
are exposed by its CTL as control elements. Open a CTL with `open/1`, list its
elements with `elem_list/1`, get information about an element with
`elem_info/2`, read and write the value of an element with `elem_read/2` and
`elem_write/3`, and close the CTL with `close/1`.

The CTL handle is closed automatically if the process that opened it
terminates.
""".
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
    ctl/0,
    elem_id/0,
    elem_id_matcher/0,
    elem_info/0,
    elem_value/0
]).

-on_load(init/0).


%%====================================================================
%% Types
%%====================================================================

-doc "A handle to a CTL.".
-opaque ctl() :: reference().

-doc """
Identifies a control element.

  * `numid` - the numeric identifier of the element
  * `interface` - the interface the element belongs to
  * `name` - the name of the element
  * `index` - the index of the element
  * `device` - the device number of the element
  * `subdevice` - the subdevice number of the element
""".
-type elem_id() :: #{
    numid := integer(),
    interface := card | mixer | pcm | rawmidi | timer | sequencer,
    name := string(),
    index := non_neg_integer(),
    device := non_neg_integer(),
    subdevice := non_neg_integer()
}.

-doc "Selects a control element, either by its numeric identifier or by its name.".
-type elem_id_matcher() :: #{numid := integer()} | #{name := string()}.

-doc """
Describes a control element. `count` is the number of values of the element,
and `type` is the type of those values:

  * `boolean` - each value is `true` or `false`
  * `integer` - each value is an integer between `min` and `max`
  * `enumerated` - each value is one of the names in `items`
  * `bytes` - the value is a binary of `count` bytes
""".
-type elem_info() ::
      #{type := boolean, count := non_neg_integer()}
    | #{type := integer, count := non_neg_integer(), min := integer(), max := integer()}
    | #{type := enumerated, count := non_neg_integer(), items := [string()]}
    | #{type := bytes, count := non_neg_integer()}.

-doc """
The value of a control element. The value of an element of type `bytes` is a
binary of `count` bytes. The value of any other element is a tuple with `count`
entries, which are booleans, integers, or item names (strings) depending on
the type of the element.
""".
-type elem_value() :: binary() | tuple().


-doc """
Opens a CTL. The returned CTL handle will be closed automatically if the
opening process terminates.

  * `Name` - ASCII identifier of the CTL handle

Returns a handle to the CTL device on success otherwise an error.
""".
-spec open(string() | binary()) -> {ok, ctl()} | {error, Error}
    when Error :: enomem | pos_integer().
open(Name) ->
    open_nif(unicode:characters_to_list(Name, latin1)).

%% nif
open_nif(_Device) ->
    erlang:nif_error(not_loaded).


-doc """
Closes a previously opened CTL handle.

  * `CTL` - the CTL handle to close

Returns `ok` on success otherwise an error.
""".
-spec close(ctl()) -> ok | {error, Error}
    when Error :: closed | pos_integer().
close(CTL) ->
    close_nif(CTL).

%% nif
close_nif(_CTL) ->
    erlang:nif_error(not_loaded).


-doc """
Lists the control elements of a CTL.

  * `CTL` - the CTL handle

Returns the identifiers of the elements on success otherwise an error.
""".
-spec elem_list(ctl()) -> {ok, [elem_id()]} | {error, Error}
    when Error :: pos_integer().
elem_list(CTL) ->
    elem_list_nif(CTL).

%% nif
elem_list_nif(_CTL) ->
    erlang:nif_error(not_loaded).


-doc """
Gets information about a control element.

  * `CTL` - the CTL handle
  * `Id` - selects the element

Returns the information about the element on success otherwise an error. The
error is `enotsup` when the type of the element is not supported.
""".
-spec elem_info(ctl(), elem_id_matcher()) -> {ok, elem_info()} | {error, Error}
    when Error :: pos_integer() | enotsup.
elem_info(CTL, Id) ->
    elem_info_nif(CTL, Id).

%% nif
elem_info_nif(_CTL, _Id) ->
    erlang:nif_error(not_loaded).


-doc """
Gets the value of a control element.

  * `CTL` - the CTL handle
  * `Id` - selects the element

Returns the value of the element on success otherwise an error. The error is
`enotsup` when the type of the element is not supported.
""".
-spec elem_read(ctl(), elem_id_matcher()) -> {ok, elem_value()} | {error, Error}
    when Error :: pos_integer() | enotsup.
elem_read(CTL, Id) ->
    elem_read_nif(CTL, Id).

%% nif
elem_read_nif(_CTL, _Id) ->
    erlang:nif_error(not_loaded).


-doc """
Sets the value of a control element. Integer values are limited to the range
of the element.

  * `CTL` - the CTL handle
  * `Id` - selects the element
  * `Value` - the new value of the element

Returns `ok` on success otherwise an error. The error is `enotsup` when the
type of the element is not supported.
""".
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
