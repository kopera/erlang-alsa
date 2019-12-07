-module(alsa_pcm).
-export([
    open/2,
    close/1,
    set_hwparams/2,
    set_swparams/2,
    set_params/2,
    prepare/1,
    drop/1,
    pause/1,
    unpause/1,
    recover/2,
    reset/1,
    resume/2,
    writei/4,
    readi/3
]).
-export([
    frame_size/1,
    sample_size/1
]).
-export_type([
    pcm/0
]).

-on_load(init/0).


-spec open(string() | binary(), playback | capture) -> {ok, pcm()} | {error, error()}.
-opaque pcm() :: reference().
open(Device, Direction) ->
    open_nif(unicode:characters_to_list(Device), case Direction of
        playback -> 0;
        capture -> 1
    end).

%% nif
open_nif(_Device, _Direction) ->
    erlang:nif_error(not_loaded).


-spec close(pcm()) -> ok | {error, error()}.
close(PCM) ->
    close_nif(PCM).

%% nif
close_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-spec set_hwparams(pcm(), hwparams()) -> ok | {error, error()}.
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
set_hwparams(PCM, Params) ->
    set_hwparams_nif(PCM, maps:to_list(Params)).

%% nif
set_hwparams_nif(_PCM, _Params) ->
    erlang:nif_error(not_loaded).


-spec set_swparams(pcm(), swparams()) -> ok | {error, error()}.
-type swparams() :: #{
    avail_min => pos_integer(),
    start_threshold => pos_integer(),
    stop_threshold => pos_integer()
}.
set_swparams(PCM, Params) ->
    set_swparams_nif(PCM, maps:to_list(Params)).

%% nif
set_swparams_nif(_PCM, _Params) ->
    erlang:nif_error(not_loaded).


-spec set_params(pcm(), params()) -> ok | {error, error()}.
-type params() :: #{
    access := hwparam_access(),
    format := hwparam_format(),
    channels := pos_integer(),
    rate := pos_integer(),
    rate_resample := boolean(),
    latency := pos_integer()
}.
set_params(PCM, Params) ->
    #{
        format := Format,
        access := Access,
        channels := Channels,
        rate := Rate,
        rate_resample := Resample,
        latency := Latency
    } = Params,
    set_params_nif(PCM, Format, Access, Channels, Rate, Resample, Latency).

%% nif
set_params_nif(_PCM, _Format, _Access, _Channels, _Rate, _Resample, _Latency) ->
    erlang:nif_error(not_loaded).


-spec prepare(pcm()) -> ok | {error, error()}.
prepare(PCM) ->
    prepare_nif(PCM).

%% nif
prepare_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-spec drop(pcm()) -> ok | {error, error()}.
drop(PCM) ->
    drop_nif(PCM).

%% nif
drop_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-spec pause(pcm()) -> ok | {error, error()}.
pause(PCM) ->
    pause_nif(PCM, true).

-spec unpause(pcm()) -> ok | {error, error()}.
unpause(PCM) ->
    pause_nif(PCM, false).

%% nif
pause_nif(_PCM, _Pause) ->
    erlang:nif_error(not_loaded).


-spec recover(pcm(), error()) -> ok | {error, error()}.
-type error() ::
      eagain | ebadfd | eintr | enoent | enosys | epipe | estrpipe
    | pos_integer().
recover(PCM, Error) ->
    recover_nif(PCM, Error, true).

%% nif
recover_nif(_PCM, _Error, _Silent) ->
    erlang:nif_error(not_loaded).


-spec reset(pcm()) -> ok | {error, error()}.
reset(PCM) ->
    reset_nif(PCM).

%% nif
reset_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-spec resume(pcm(), timeout()) -> ok | {error, error()}.
resume(PCM, Timeout) ->
    ResumeRef = make_ref(),
    resume(PCM, ResumeRef, Timeout).

resume(PCM, ResumeRef, Timeout) ->
    StartTime = timestamp(),
    case resume_nif(PCM, ResumeRef) of
        ok ->
            ok;
        {wait, ResumeRef} ->
            NewTimeout = next_timeout(StartTime, Timeout),
            receive
                {select, PCM, ResumeRef, _} ->
                    resume(PCM, ResumeRef, next_timeout(StartTime, Timeout))
            after
                NewTimeout ->
                    resume_cancel(PCM, ResumeRef),
                    {error, timeout}
            end;
        {error, _} = Error ->
            Error
    end.

resume_cancel(_PCM, _ResumeRef) ->
    % case select_cancel_nif(PCM, ResumeRef) of
    %     {error, select_sent} ->
    %         select_flush(PCM, ResumeRef);
    %     Other ->
    %         Other
    % end.
    ok.


%% nif
resume_nif(_PCM, _Ref) ->
    erlang:nif_error(not_loaded).


-spec writei(pcm(), binary(), pos_integer(), timeout()) -> ok | {error, error()}.
writei(PCM, Buffer, Frames, Timeout) ->
    WriteRef = make_ref(),
    writei(PCM, Buffer, Frames, WriteRef, Timeout).

writei(PCM, Buffer, Frames, WriteRef, Timeout) ->
    StartTime = timestamp(),
    case writei_nif(PCM, Buffer, Frames, WriteRef) of
        {ok, FramesWritten, _BytesWritten} when FramesWritten =:= Frames ->
            ok;
        {ok, FramesWritten, BytesWritten} when FramesWritten < Frames ->
            <<_:BytesWritten/binary, BufferRemaining/binary>> = Buffer,
            FramesRemaining = Frames - FramesWritten,
            writei(PCM, BufferRemaining, FramesRemaining, WriteRef,
                next_timeout(StartTime, Timeout));
        {wait, WriteRef} ->
            ReceiveTimeout = next_timeout(StartTime, Timeout),
            receive
                {select, PCM, WriteRef, _} ->
                    writei(PCM, Buffer, Frames, WriteRef,
                        next_timeout(StartTime, Timeout))
            after
                ReceiveTimeout ->
                    writei_cancel(PCM, WriteRef),
                    {error, timeout}
            end;
        wait ->
            timer:sleep(1),
            writei(PCM, Buffer, Frames, WriteRef,
                next_timeout(StartTime, Timeout));
        {error, _} = Error ->
            Error
    end.

writei_cancel(_PCM, _WriteRef) ->
    % case select_cancel_nif(PCM, WriteRef) of
    %     {error, select_sent} ->
    %         select_flush(PCM, WriteRef);
    %     Other ->
    %         Other
    % end.
    ok.

%% nif
writei_nif(_PCM, _Buffer, _Frames, _Ref) ->
    erlang:nif_error(not_loaded).


-spec readi(pcm(), pos_integer(), timeout()) -> {ok, binary(), pos_integer()} | {error, error()}.
readi(PCM, Frames, Timeout) when Frames > 0 ->
    ReadRef = make_ref(),
    readi(PCM, Frames, ReadRef, <<>>, 0, Timeout).

readi(PCM, Frames, ReadRef, DataAcc, FramesAcc, Timeout) ->
    StartTime = timestamp(),
    case readi_nif(PCM, Frames, ReadRef) of
        {ok, FramesRead, Data} when FramesRead =:= Frames ->
            {ok, <<DataAcc/binary, Data/binary>>, FramesAcc + FramesRead};
        {ok, FramesRead, Data} when FramesRead < Frames ->
            readi(PCM, Frames - FramesRead, ReadRef, <<DataAcc/binary, Data/binary>>, FramesAcc + FramesRead,
                next_timeout(StartTime, Timeout));
        {wait, ReadRef} ->
            NewTimeout = next_timeout(StartTime, Timeout),
            receive
                {select, PCM, ReadRef, _} ->
                    readi(PCM, Frames, ReadRef, DataAcc, FramesAcc,
                        next_timeout(StartTime, Timeout))
            after
                NewTimeout ->
                    readi_cancel(PCM, ReadRef),
                    {error, timeout}
            end;
        wait ->
            timer:sleep(1),
            readi(PCM, Frames, ReadRef, DataAcc, FramesAcc,
                next_timeout(StartTime, Timeout));
        {error, _} = Error ->
            Error
    end.

readi_cancel(_PCM, _ReadRef) ->
    % case select_cancel_nif(PCM, ReadRef) of
    %     {error, select_sent} ->
    %         select_flush(PCM, ReadRef);
    %     Other ->
    %         Other
    % end.
    ok.

%% nif
readi_nif(_PCM, _Frames, _Ref) ->
    erlang:nif_error(not_loaded).


-spec frame_size(pcm()) -> pos_integer().
frame_size(PCM) ->
    frames_to_bytes_nif(PCM, 1).

%% nif
frames_to_bytes_nif(_PCM, _Frames) ->
    erlang:nif_error(not_loaded).


-spec sample_size(pcm()) -> pos_integer().
sample_size(PCM) ->
    samples_to_bytes_nif(PCM, 1).

%% nif
samples_to_bytes_nif(_PCM, _Frames) ->
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
    filename:join([Dir, "alsa_pcm_nif"]).
