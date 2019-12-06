%% @private
-module(alsa_mixer_port).
-export([
    open/1,
    close/1,
    send/2
]).


-spec open([string() | binary()]) -> port().
open(Args) ->
    case path() of
        undefined ->
            exit(unsupported_platform);
        Path ->
            open_port({spawn_executable, Path}, [
                {args, Args},
                {packet, 2},
                binary,
                use_stdio,
                exit_status
            ])
    end.


-spec close(port()) -> boolean().
close(Port) ->
    port_close(Port).


-spec send(port(), term()) -> term().
send(Port, Message) ->
    true = port_command(Port, term_to_binary(Message)),
    Message.


-spec path() -> string() | binary() | undefined.
path() ->
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
    path(os:type(), Priv).

path({unix, linux}, Dir) ->
    filename:join([Dir, "alsa_mixer_port"]);
path(_, _Dir) ->
    undefined.
