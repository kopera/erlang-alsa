-module(alsa).
-export([
    pcm_open/2,
    pcm_close/1,
    pcm_set_hwparams/2,
    pcm_set_swparams/2,
    pcm_set_params/2,
    pcm_prepare/1,
    pcm_pause/1,
    pcm_unpause/1,
    pcm_recover/2,
    pcm_reset/1,
    pcm_resume/2,
    pcm_writei/4
]).
-export_type([
    pcm/0
]).

-on_load(init/0).


-spec pcm_open(string() | binary(), playback | capture) -> {ok, pcm()} | {error, error()}.
-opaque pcm() :: reference().
pcm_open(Device, Direction) ->
    pcm_open_nif(unicode:characters_to_list(Device), case Direction of
        playback -> 0;
        capture -> 1
    end).

%% nif
pcm_open_nif(_Device, _Direction) ->
    erlang:nif_error(not_loaded).


-spec pcm_close(pcm()) -> ok | {error, error()}.
pcm_close(PCM) ->
    pcm_close_nif(PCM).

%% nif
pcm_close_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-spec pcm_set_hwparams(pcm(), hwparams()) -> ok | {error, error()}.
-type hwparams() :: #{
    access => hwparam_access(),
    format => hwparam_format(),
    channels => pos_integer(),
    rate => pos_integer(),
    rate_resample => boolean()
}.
-type hwparam_access() :: rw_interleaved | rw_noninterleaved.
-type hwparam_format() ::
      s8 | u8
    | s16_le | s16_be | u16_le | u16_be
    | s24_le | s24_be | u24_le | u24_be
    | s32_le | s32_be | u32_le | u32_be
    | float_le | float_be
    | float64_le | float64_be
    | iec958_subframe_le | iec958_subframe_be
    | mu_law
    | a_law
    | ima_adpcm
    | mpeg
    | gsm
    | s20_le | s20_be | u20_le | u20_be
    | special
    | s24_3le | s24_3be | u24_3le | u24_3be
    | s20_3le | s20_3be | u20_3le | u20_3be
    | s18_3le | s18_3be | u18_3le | u18_3be
    | g723_24 | g723_24_1b
    | g723_40 | g723_40_1b
    | dsd_u8
    | dsd_u16_le | dsd_u16_be
    | dsd_u32_le | dsd_u32_be
    % aliases
    | s16 | u16
    | s24 | u24
    | s32 | u32
    | float
    | float64
    | iec958_subframe
    | s20 | u20.
pcm_set_hwparams(PCM, Params) ->
    pcm_set_hwparams_nif(PCM, maps:to_list(Params)).

%% nif
pcm_set_hwparams_nif(_PCM, _Params) ->
    erlang:nif_error(not_loaded).


-spec pcm_set_swparams(pcm(), swparams()) -> ok | {error, error()}.
-type swparams() :: #{
    avail_min => pos_integer(),
    start_threshold => pos_integer(),
    stop_threshold => pos_integer()
}.
pcm_set_swparams(PCM, Params) ->
    pcm_set_swparams_nif(PCM, maps:to_list(Params)).

%% nif
pcm_set_swparams_nif(_PCM, _Params) ->
    erlang:nif_error(not_loaded).


-spec pcm_set_params(pcm(), params()) -> ok | {error, error()}.
-type params() :: #{
    access := hwparam_access(),
    format := hwparam_format(),
    channels := pos_integer(),
    rate := pos_integer(),
    rate_resample := boolean(),
    latency := pos_integer()
}.
pcm_set_params(PCM, Params) ->
    #{
        format := Format,
        access := Access,
        channels := Channels,
        rate := Rate,
        rate_resample := Resample,
        latency := Latency
    } = Params,
    pcm_set_params_nif(PCM, Format, Access, Channels, Rate, Resample, Latency).

%% nif
pcm_set_params_nif(_PCM, _Format, _Access, _Channels, _Rate, _Resample, _Latency) ->
    erlang:nif_error(not_loaded).


-spec pcm_prepare(pcm()) -> ok | {error, error()}.
pcm_prepare(PCM) ->
    pcm_prepare_nif(PCM).

%% nif
pcm_prepare_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-spec pcm_pause(pcm()) -> ok | {error, error()}.
pcm_pause(PCM) ->
    pcm_pause_nif(PCM, true).

-spec pcm_unpause(pcm()) -> ok | {error, error()}.
pcm_unpause(PCM) ->
    pcm_pause_nif(PCM, false).

%% nif
pcm_pause_nif(_PCM, _Pause) ->
    erlang:nif_error(not_loaded).


-spec pcm_recover(pcm(), error()) -> ok | {error, error()}.
-type error() ::
      eagain | ebadfd | eintr | enoent | enosys | epipe | estrpipe
    | pos_integer().
pcm_recover(PCM, Error) ->
    pcm_recover_nif(PCM, Error, true).

%% nif
pcm_recover_nif(_PCM, _Error, _Silent) ->
    erlang:nif_error(not_loaded).


-spec pcm_reset(pcm()) -> ok | {error, error()}.
pcm_reset(PCM) ->
    pcm_reset_nif(PCM).

%% nif
pcm_reset_nif(_PCM) ->
    erlang:nif_error(not_loaded).


pcm_resume(PCM, Timeout) ->
    ResumeRef = make_ref(),
    pcm_resume(PCM, ResumeRef, Timeout).

pcm_resume(PCM, ResumeRef, Timeout) ->
    StartTime = timestamp(),
    case pcm_resume_nif(PCM, ResumeRef) of
        ok ->
            ok;
        {wait, ResumeRef} ->
            NewTimeout = next_timeout(StartTime, Timeout),
            receive
                {select, PCM, ResumeRef, _} ->
                    pcm_resume(PCM, ResumeRef, next_timeout(StartTime, Timeout))
            after
                NewTimeout ->
                    pcm_resume_cancel(PCM, ResumeRef),
                    {error, timeout}
            end;
        {error, _} = Error ->
            Error
    end.

pcm_resume_cancel(_PCM, _ResumeRef) ->
    % case select_cancel_nif(PCM, ResumeRef) of
    %     {error, select_sent} ->
    %         select_flush(PCM, ResumeRef);
    %     Other ->
    %         Other
    % end.
    ok.


%% nif
pcm_resume_nif(_PCM, _Ref) ->
    erlang:nif_error(not_loaded).


pcm_writei(PCM, Buffer, Frames, Timeout) ->
    WriteRef = make_ref(),
    pcm_writei(PCM, Buffer, Frames, WriteRef, Timeout).

pcm_writei(PCM, Buffer, Frames, WriteRef, Timeout) ->
    StartTime = timestamp(),
    case pcm_writei_nif(PCM, Buffer, Frames, WriteRef) of
        ok ->
            ok;
        {wait, WriteRef, FramesWritten, BytesWritten} ->
            NewTimeout = next_timeout(StartTime, Timeout),
            receive
                {select, PCM, WriteRef, _} ->
                    <<_:BytesWritten/binary, BufferRest/binary>> = Buffer,
                    pcm_writei(PCM,
                        BufferRest,
                        Frames - FramesWritten,
                        WriteRef,
                        next_timeout(StartTime, Timeout))
            after
                NewTimeout ->
                    pcm_writei_cancel(PCM, WriteRef),
                    {error, timeout}
            end;
        {error, _} = Error ->
            Error
    end.

pcm_writei_cancel(_PCM, _WriteRef) ->
    % case select_cancel_nif(PCM, WriteRef) of
    %     {error, select_sent} ->
    %         select_flush(PCM, WriteRef);
    %     Other ->
    %         Other
    % end.
    ok.

%% nif
pcm_writei_nif(_PCM, _Buffer, _Frames, _Ref) ->
    erlang:nif_error(not_loaded).


%
% Helpers
%

timestamp() ->
    erlang:monotonic_time(milli_seconds).


next_timeout(_, infinity = Timeout) ->
    Timeout;

next_timeout(StartTimestamp, Timeout) ->
    Elapsed = timestamp() - StartTimestamp,
    case Timeout - Elapsed of
        NewTimeout when NewTimeout > 0 ->
            NewTimeout;
        _ ->
            0
    end.


% select_flush(PCM, Ref) ->
%     receive
%         {select, PCM, Ref, _} ->
%             select_flush(PCM, Ref)
%     after
%         0 ->
%             ok
%     end.

%
% Initialization
%

init() ->
    case nif_path() of
        undefined ->
            ok;
        Path ->
            ok = erlang:load_nif(Path, 0)
    end.

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
    filename:join([Dir, "alsa_nif"]).
