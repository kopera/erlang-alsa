-module(alsa_pcm).
-export([
    open/2,
    close/1,
    set_hwparams/2,
    set_swparams/2,
    get_swparams/1,
    set_params/2,
    prepare/1,
    drop/1,
    pause/1,
    unpause/1,
    recover/2,
    reset/1,
    resume/2,
    start/1,
    writei/3,
    readi/2
]).
-export([
    frame_size/1,
    sample_size/1
]).
-export_type([
    pcm/0,
    error/0
]).

-on_load(init/0).


%% @doc Opens a PCM. The returned PCM handle will be closed automatically if the
%% opening process terminates.
%%
%% @param Device    ASCII identifier of the PCM handle
%% @param Direction wanted stream direction, that is either `capture' for
%%                  reading audio data from the audio device, or `playback' for
%%                  writing audio data to the audio device.
%% @returns a handle to the PCM device on success otherwise a posix error.
-spec open(string() | binary(), playback | capture) -> {ok, pcm()} | {error, error()}.
-opaque pcm() :: reference().
open(Device, Direction) ->
    open_nif(unicode:characters_to_list(Device, latin1), case Direction of
        playback -> 0;
        capture -> 1
    end).

%% nif
open_nif(_Device, _Direction) ->
    erlang:nif_error(not_loaded).


%% @doc Closes a previously opened PCM handle.
%% @param PCM       the PCM handle to close
%% @returns ok on success otherwise a posix error.
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


%% @doc Set the software parameters
%%
%% <dl>
%%     <dt>avail_min</dt>
%%     <dd>Set avail min inside a software configuration container. The valid
%%      values are determined by the specific hardware. Most PC sound cards
%%      can only accept power of 2 frame counts (i.e. 512, 1024, 2048).</dd>
%%
%%     <dt>start_threshold</dt>
%%     <dd>PCM is automatically started when playback frames available to PCM
%%     are >= `start_threshold` or when requested capture frames are >= `start_threshold`.</dd>
%%
%%     <dt>stop_threshold</dt>
%%     <dd>PCM is automatically stopped an underrun/overrun state when available frames is >= `stop_threshold`.
%%     If the stop threshold is equal to boundary then automatic stop will be disabled (thus device will do
%%     the endless loop in the ring buffer).</dd>
%%
%%     <dt>silence_threshold</dt>
%%     <dd>A portion of playback buffer is overwritten with silence when playback underrun is
%%     nearer than silence threshold.</dd>
%%
%%     <dt>silence_size</dt>
%%     <dd>A portion of playback buffer is overwritten with silence when playback underrun is nearer
%%     than silence threshold (see snd_pcm_sw_params_set_silence_threshold) The special case is when
%%     silence size value is equal or greater than boundary. The unused portion of the ring buffer
%%     (initial written samples are untouched) is filled with silence at start. Later, only just
%%     processed sample area is filled with silence. Note: silence_threshold must be set to zero.</dd>
%%
%% </dl>
%%
%% @param PCM       the PCM handle
%% @param Params    a map of the parameters to set.
%% @returns ok on success otherwise a posix error.
-spec set_swparams(pcm(), swparams_options()) -> ok | {error, error()}.
-type swparams_options() :: #{
    avail_min => pos_integer(),
    start_threshold => pos_integer(),
    stop_threshold => pos_integer(),
    silence_threshold => pos_integer(),
    silence_size => pos_integer()
}.
set_swparams(PCM, Params) ->
    set_swparams_nif(PCM, maps:to_list(Params)).

%% nif
set_swparams_nif(_PCM, _Params) ->
    erlang:nif_error(not_loaded).


-spec get_swparams(pcm()) -> {ok, swparams()} | {error, error()}.
-type swparams() :: #{
    avail_min := pos_integer(),
    boundary := pos_integer(),
    start_threshold := pos_integer(),
    stop_threshold := pos_integer(),
    silence_threshold := pos_integer(),
    silence_size := pos_integer()
}.
get_swparams(PCM) ->
    get_swparams_nif(PCM).

%% nif
get_swparams_nif(_PCM) ->
    erlang:nif_error(not_loaded).


%% @doc Set the hardware and software parameters in a simple way. The following
%%      parameters are all required:
%%
%% <dl>
%%     <dt>access</dt>
%%     <dd>The required {@link hwparam_access(). access} mode, that is either
%%     rw_interleaved or rw_noninterleaved. Interleaved access means that the
%%     channels data is multiplexed into the same buffer, and that {@link readi/2}
%%     and {@link writei/3} are to be used. Non interleaves means that the channels
%%     data is read/written to separate buffers, in which case {@link readn/2} and
%%     {@link writen/3} are to be used.</dd>
%%
%%     <dt>format</dt>
%%     <dd>The {@link hwparam_format(). sample format}. The format specifies how
%%     the data in each sample is encoded.</dd>
%%
%%     <dt>channels</dt>
%%     <dd>The number of channels. Must be a stictly positive integer.</dd>
%%
%%     <dt>rate</dt>
%%     <dd>The sample rate, such as 44100 or 48000.</dd>
%%
%%     <dt>rate_resample</dt>
%%     <dd>Whether to allow software resampling in case the provided sample rate
%%     is not supported by the target device.</dd>
%%
%%     <dt>latency</dt>
%%     <dd>Required overall latency in μs.</dd>
%% </dl>
%%
%% @param PCM       the PCM handle
%% @param Params    a map of the parameters to set.
%% @returns ok on success otherwise a posix error.
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


%% @doc Prepare PCM for use.
%% @param PCM       the PCM handle to prepare
%% @returns ok on success otherwise a posix error.
-spec prepare(pcm()) -> ok | {error, error()}.
prepare(PCM) ->
    prepare_nif(PCM).

%% nif
prepare_nif(_PCM) ->
    erlang:nif_error(not_loaded).


%% @doc Stop a PCM dropping pending frames. This function stops the PCM
%%      immediately. The pending samples on the buffer are ignored.
%%
%% @param PCM       the PCM handle
%% @returns ok on success otherwise a posix error.
-spec drop(pcm()) -> ok | {error, error()}.
drop(PCM) ->
    drop_nif(PCM).

%% nif
drop_nif(_PCM) ->
    erlang:nif_error(not_loaded).


%% @doc Pause the PCM. Note that this function works only on the hardware which
%%      supports pause feature.
%%
%% @param PCM       the PCM handle
%% @returns ok on success otherwise a posix error.
-spec pause(pcm()) -> ok | {error, error()}.
pause(PCM) ->
    pause_nif(PCM, true).


%% @doc Unpause a previously paused PCM.
%% @see pause/1
%% @param PCM       the PCM handle
%% @returns ok on success otherwise a posix error.
-spec unpause(pcm()) -> ok | {error, error()}.
unpause(PCM) ->
    pause_nif(PCM, false).

%% nif
pause_nif(_PCM, _Pause) ->
    erlang:nif_error(not_loaded).


%% @doc Recover the stream state from an error or suspend. This functions
%%      handles eintr (interrupted system call), epipe (overrun or underrun) and
%%      estrpipe (stream is suspended) error codes trying to prepare given
%%      stream for next I/O.
%%
%%      Note that this function returs the original error code when it is not
%%      handled inside this function.
%%
%% @param PCM       the PCM handle
%% @param Error     the previously returned error from which we are trying to
%%                  recover.
%% @returns ok on success otherwise a posix error.
-spec recover(pcm(), error()) -> ok | {error, error()}.
-type error() ::
      eagain | ebadfd | eintr | enoent | enosys | epipe | estrpipe
    | pos_integer().
recover(PCM, Error) ->
    recover_nif(PCM, Error, true).

%% nif
recover_nif(_PCM, _Error, _Silent) ->
    erlang:nif_error(not_loaded).


%% @hidden
%% @doc Reset the PCM position. This function resets the buffer and thus delay
%%      to 0 while keeping the playback running. This will most propably result
%%      in an underrun.
%%
%% @param PCM       the PCM handle
%% @returns ok on success otherwise a posix error.
-spec reset(pcm()) -> ok | {error, error()}.
reset(PCM) ->
    reset_nif(PCM).

%% nif
reset_nif(_PCM) ->
    erlang:nif_error(not_loaded).


%% @hidden
%% @doc Resume from suspend, no samples are lost. This function can be used when
%%      the stream is in the suspend state to do the fine resume from this
%%      state. Not all hardware supports this feature, when an enosys error is
%%      returned, use the @{link prepare/1} function to recover.
%%
%% @param PCM       the PCM handle
%% @param Timeout   a timeout or `nowait'
%% @returns ok on success otherwise a posix error. If `nowait' is specified
%%          as timeout, this function will return `{wait, {ready_resume, Reference}}',
%%          at which point, the caller should wait for the `{ready_resume, Reference}'
%%          message to be delivered to its message box before calling this
%%          function again.
-spec resume(pcm(), timeout() | nowait) ->
      ok
    | {wait, {ready_resume, reference()}}
    | {error, timeout}
    | {error, error()}.
resume(PCM, Timeout) ->
    StartTime = timestamp(),
    ResumeRef = make_ref(),
    ReadyResumeMessage = {ready_resume, ResumeRef},
    case resume_nif(PCM, ReadyResumeMessage) of
        ok ->
            ok;
        wait when Timeout =:= nowait ->
            {wait, ReadyResumeMessage};
        wait ->
            NewTimeout = next_timeout(StartTime, Timeout),
            receive
                ReadyResumeMessage ->
                    resume(PCM, next_timeout(StartTime, Timeout))
            after
                NewTimeout ->
                    resume_cancel(PCM, ResumeRef),
                    {error, timeout}
            end;
        {error, _} = Error ->
            Error
    end.

resume_cancel(_PCM, _ResumeRef) ->
    % case cancel_nif(PCM, ResumeRef) of
    %     {error, select_sent} ->
    %         select_flush(PCM, ResumeRef);
    %     Other ->
    %         Other
    % end.
    ok.


%% nif
resume_nif(_PCM, _Ref) ->
    erlang:nif_error(not_loaded).


%% @doc Start a PCM. Normally you don't need to call this function for playback
%%      if using {@link set_params/2} as the auto-start mechanism will kick in.
%%      However, you need to call this function in capture mode.
%%
%% @param PCM       the PCM handle
%% @returns ok on success otherwise a posix error.
-spec start(pcm()) -> ok | {error, error()}.
start(PCM) ->
    start_nif(PCM).

%% nif
start_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-spec writei(pcm(), binary(), timeout() | nowait) ->
      ok
    | {wait, {ready_writei, reference()}, Remaining :: binary()}
    | {error, incomplete_frame, Remaining :: binary()}
    | {error, timeout, Remaining :: binary()}
    | {error, error(), Remaining :: binary()}.
writei(PCM, Buffer, Timeout) ->
    FrameSize = frame_size(PCM),
    BufferSize = byte_size(Buffer),
    case BufferSize rem FrameSize of
        0 ->
            do_writei(PCM, FrameSize, Buffer, BufferSize div FrameSize, Timeout);
        TrailingSize ->
            % Incomplete frame situation
            BufferWritableSize = BufferSize - TrailingSize,
            <<BufferWritable:BufferWritableSize/binary, Trailing:TrailingSize/binary>> = Buffer,
            case do_writei(PCM, FrameSize, BufferWritable, BufferSize div FrameSize, Timeout) of
                ok ->
                    {error, incomplete_frame, Trailing};
                {wait, Message, Remaining} ->
                    {wait, Message, <<Remaining/binary, Trailing/binary>>};
                {error, Error, Remaining} ->
                    {error, Error, <<Remaining/binary, Trailing/binary>>}
            end
    end.

do_writei(_PCM, _FrameSize, <<>>, 0, _Timeout) ->
    ok;
do_writei(PCM, FrameSize, Buffer, Frames, Timeout) ->
    StartTime = timestamp(),
    WriteRef = make_ref(),
    ReadyWriteMessage = {ready_writei, WriteRef},
    case writei_nif(PCM, Buffer, Frames, ReadyWriteMessage) of
        {ok, FramesWritten} when FramesWritten =:= Frames ->
            ok;
        {ok, FramesWritten} when FramesWritten < Frames ->
            BytesWritten = FramesWritten * FrameSize,
            <<_:BytesWritten/binary, BufferRemaining/binary>> = Buffer,
            FramesRemaining = Frames - FramesWritten,
            do_writei(PCM, FrameSize, BufferRemaining, FramesRemaining, next_timeout(StartTime, Timeout));
        wait when Timeout =:= nowait ->
            {wait, ReadyWriteMessage, Buffer};
        wait ->
            ReceiveTimeout = next_timeout(StartTime, Timeout),
            receive
                ReadyWriteMessage ->
                    do_writei(PCM, FrameSize, Buffer, Frames, next_timeout(StartTime, Timeout))
            after
                ReceiveTimeout ->
                    writei_cancel(PCM, WriteRef),
                    {error, timeout, Buffer}
            end;
        {error, Error} ->
            {error, Error, Buffer}
    end.

writei_cancel(_PCM, _WriteRef) ->
    % case cancel_nif(PCM, WriteRef) of
    %     {error, select_sent} ->
    %         select_flush(PCM, WriteRef);
    %     Other ->
    %         Other
    % end.
    ok.

%% nif
writei_nif(_PCM, _Buffer, _Frames, _Ref) ->
    erlang:nif_error(not_loaded).


-spec readi(pcm(), timeout() | nowait) ->
      {ok, binary()}
    | {wait, {ready_readi, reference()}}
    | {error, timeout}
    | {error, error()}.
readi(PCM, Timeout) ->
    StartTime = timestamp(),
    ReadRef = make_ref(),
    ReadyReadMessage = {ready_readi, ReadRef},
    case readi_nif(PCM, 4096, ReadyReadMessage) of
        {ok, Data, _FramesRead} ->
            {ok, Data};
        wait when Timeout =:= nowait ->
            {wait, ReadyReadMessage};
        wait ->
            NewTimeout = next_timeout(StartTime, Timeout),
            receive
                ReadyReadMessage ->
                    readi(PCM, next_timeout(StartTime, Timeout))
            after
                NewTimeout ->
                    readi_cancel(PCM, ReadRef),
                    {error, timeout}
            end;
        {error, _} = Error ->
            Error
    end.

readi_cancel(_PCM, _ReadRef) ->
    % case cancel_nif(PCM, ReadRef) of
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

next_timeout(_, nowait = Timeout) ->
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
