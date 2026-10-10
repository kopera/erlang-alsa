-module(alsa_pcm).
-moduledoc """
Capture and playback of audio through an ALSA PCM device.

Open a device with `open/2`, configure it with `set_params/2` (or with
`set_hwparams/2` and `set_swparams/2` for finer control), then play audio with
`writei/3` or capture it with `readi/2`, and close the device with `close/1`.
The PCM handle is closed automatically if the process that opened it
terminates.

The functions return `{error, Error}` with a posix error `Error`, see
`t:error/0`. `recover/2` recovers a stream from the errors that can be
recovered from, such as an underrun or an overrun.

## Example

Playing audio:

```erlang
{ok, PCM} = alsa_pcm:open("default", playback),
ok = alsa_pcm:set_params(PCM, #{
    format => s16_le,
    access => rw_interleaved,
    channels => 2,
    rate => 48000,
    rate_resample => true,
    latency => 100000
}),
ok = alsa_pcm:writei(PCM, Data, infinity),
ok = alsa_pcm:close(PCM).
```

See the `examples` directory for more.
""".
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
    error/0,
    hwparams/0,
    hwparam_access/0,
    hwparam_format/0,
    params/0,
    swparams_options/0,
    swparams/0
]).

-on_load(init/0).


%%====================================================================
%% Types
%%====================================================================

-doc "A handle to a PCM.".
-opaque pcm() :: reference().

-doc """
A posix error. The most common ones are atoms, the others are error numbers.
""".
-type error() ::
      eagain | ebadfd | ebusy | eintr | enoent | enosys | epipe | estrpipe
    | pos_integer().

-doc """
The access mode of the PCM: `rw_interleaved` when the channels data is
multiplexed into the same buffer, or `rw_noninterleaved` when each channel has
a separate buffer.
""".
-type hwparam_access() :: rw_interleaved | rw_noninterleaved.

-doc """
The sample format. The names are those of the ALSA sample formats, such as
`s16_le` for signed 16 bit little endian samples and `float_be` for 32 bit
float big endian samples. The names without an endianness (`s16`, `u16`, `s24`,
`u24`, `s32`, `u32`, `float`, `float64`, `iec958_subframe`, `s20` and `u20`)
are aliases for the endianness of the machine.
""".
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

-doc """
The hardware parameters of a PCM, see `set_hwparams/2`.

  * `access` - the access mode
  * `format` - the sample format
  * `channels` - the number of channels
  * `rate` - the sample rate, such as 44100 or 48000
  * `rate_resample` - whether to allow software resampling in case the
    provided sample rate is not supported by the device
""".
-type hwparams() :: #{
    access => hwparam_access(),
    format => hwparam_format(),
    channels => pos_integer(),
    rate => pos_integer(),
    rate_resample => boolean()
}.

-doc "The parameters for `set_params/2`, which are all required.".
-type params() :: #{
    access := hwparam_access(),
    format := hwparam_format(),
    channels := pos_integer(),
    rate := pos_integer(),
    rate_resample := boolean(),
    latency := pos_integer()
}.

-doc "The software parameters to set, see `set_swparams/2`.".
-type swparams_options() :: #{
    avail_min => pos_integer(),
    start_threshold => non_neg_integer(),
    stop_threshold => non_neg_integer(),
    silence_threshold => non_neg_integer(),
    silence_size => non_neg_integer()
}.

-doc "The current software parameters, see `get_swparams/1`.".
-type swparams() :: #{
    avail_min := pos_integer(),
    boundary := pos_integer(),
    start_threshold := non_neg_integer(),
    stop_threshold := non_neg_integer(),
    silence_threshold := non_neg_integer(),
    silence_size := non_neg_integer()
}.


-doc """
Opens a PCM. The returned PCM handle will be closed automatically if the
opening process terminates.

  * `Device` - ASCII identifier of the PCM handle
  * `Direction` - wanted stream direction, that is either `capture` for
    reading audio data from the audio device, or `playback` for writing audio
    data to the audio device

Returns a handle to the PCM device on success otherwise a posix error.
""".
-spec open(string() | binary(), playback | capture) -> {ok, pcm()} | {error, error()}.
open(Device, Direction) ->
    open_nif(unicode:characters_to_list(Device, latin1), case Direction of
        playback -> 0;
        capture -> 1
    end).

%% nif
open_nif(_Device, _Direction) ->
    erlang:nif_error(not_loaded).


-doc """
Closes a previously opened PCM handle.

  * `PCM` - the PCM handle to close

Returns `ok` on success otherwise a posix error.
""".
-spec close(pcm()) -> ok | {error, error()}.
close(PCM) ->
    close_nif(PCM).

%% nif
close_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-doc """
Sets the hardware parameters. The parameters that are not given are left for
the device to choose.

  * `PCM` - the PCM handle
  * `Params` - a map of the parameters to set, see `t:hwparams/0`

Returns `ok` on success otherwise a posix error. See `set_params/2` to set the
hardware and software parameters in a simple way.
""".
-spec set_hwparams(pcm(), hwparams()) -> ok | {error, error()}.
set_hwparams(PCM, Params) ->
    set_hwparams_nif(PCM, maps:to_list(Params)).

%% nif
set_hwparams_nif(_PCM, _Params) ->
    erlang:nif_error(not_loaded).


-doc """
Sets the software parameters.

  * `avail_min` - the minimum number of frames that must be available before
    the PCM wakes up. The valid values are determined by the specific
    hardware. Most PC sound cards can only accept power of 2 frame counts
    (i.e. 512, 1024, 2048).
  * `start_threshold` - the PCM is automatically started when the playback
    frames available to the PCM are greater than or equal to
    `start_threshold`, or when the requested capture frames are greater than
    or equal to `start_threshold`.
  * `stop_threshold` - the PCM is automatically stopped in an underrun or
    overrun state when the available frames are greater than or equal to
    `stop_threshold`. If the stop threshold is equal to the boundary then the
    automatic stop is disabled (thus the device will do the endless loop in
    the ring buffer).
  * `silence_threshold` - a portion of the playback buffer is overwritten
    with silence when the playback underrun is nearer than the silence
    threshold.
  * `silence_size` - a portion of the playback buffer is overwritten with
    silence when the playback underrun is nearer than the silence threshold
    (see `snd_pcm_sw_params_set_silence_threshold`). The special case is when
    the silence size is equal to or greater than the boundary. The unused
    portion of the ring buffer (initial written samples are untouched) is
    filled with silence at start. Later, only the just processed sample area
    is filled with silence. Note: `silence_threshold` must be set to zero.

Arguments:

  * `PCM` - the PCM handle
  * `Params` - a map of the parameters to set, see `t:swparams_options/0`

Returns `ok` on success otherwise a posix error.
""".
-spec set_swparams(pcm(), swparams_options()) -> ok | {error, error()}.
set_swparams(PCM, Params) ->
    set_swparams_nif(PCM, maps:to_list(Params)).

%% nif
set_swparams_nif(_PCM, _Params) ->
    erlang:nif_error(not_loaded).


-doc """
Gets the current software parameters.

  * `PCM` - the PCM handle

Returns the parameters, see `t:swparams/0`, on success otherwise a posix
error. The `boundary` is the position at which the PCM position wraps around.
""".
-spec get_swparams(pcm()) -> {ok, swparams()} | {error, error()}.
get_swparams(PCM) ->
    get_swparams_nif(PCM).

%% nif
get_swparams_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-doc """
Sets the hardware and software parameters in a simple way. The following
parameters are all required:

  * `access` - the access mode, see `t:hwparam_access/0`. Interleaved access
    means that the channels data is multiplexed into the same buffer, and that
    `readi/2` and `writei/3` are to be used. Non interleaved access means that
    the channels data is read or written to separate buffers, this library
    does not provide functions for it.
  * `format` - the sample format, see `t:hwparam_format/0`. The format
    specifies how the data in each sample is encoded.
  * `channels` - the number of channels, which must be a strictly positive
    integer.
  * `rate` - the sample rate, such as 44100 or 48000.
  * `rate_resample` - whether to allow software resampling in case the
    provided sample rate is not supported by the target device.
  * `latency` - the required overall latency in microseconds.

Arguments:

  * `PCM` - the PCM handle
  * `Params` - a map of the parameters to set, see `t:params/0`

Returns `ok` on success otherwise a posix error.
""".
-spec set_params(pcm(), params()) -> ok | {error, error()}.
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


-doc """
Prepares the PCM for use.

  * `PCM` - the PCM handle to prepare

Returns `ok` on success otherwise a posix error.
""".
-spec prepare(pcm()) -> ok | {error, error()}.
prepare(PCM) ->
    prepare_nif(PCM).

%% nif
prepare_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-doc """
Stops the PCM, dropping the pending frames. The PCM is stopped immediately and
the pending samples on the buffer are ignored.

  * `PCM` - the PCM handle

Returns `ok` on success otherwise a posix error.
""".
-spec drop(pcm()) -> ok | {error, error()}.
drop(PCM) ->
    drop_nif(PCM).

%% nif
drop_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-doc """
Pauses the PCM. Note that this function works only on the hardware which
supports the pause feature.

  * `PCM` - the PCM handle

Returns `ok` on success otherwise a posix error. See `unpause/1`.
""".
-spec pause(pcm()) -> ok | {error, error()}.
pause(PCM) ->
    pause_nif(PCM, true).


-doc """
Unpauses a previously paused PCM.

  * `PCM` - the PCM handle

Returns `ok` on success otherwise a posix error. See `pause/1`.
""".
-spec unpause(pcm()) -> ok | {error, error()}.
unpause(PCM) ->
    pause_nif(PCM, false).

%% nif
pause_nif(_PCM, _Pause) ->
    erlang:nif_error(not_loaded).


-doc """
Recovers the stream state from an error or a suspend. This function handles
the `eintr` (interrupted system call), `epipe` (overrun or underrun) and
`estrpipe` (stream is suspended) errors, trying to prepare the given stream for
the next I/O.

Note that this function returns the original error when it is not handled
inside this function.

  * `PCM` - the PCM handle
  * `Error` - the previously returned error from which we are trying to
    recover

Returns `ok` on success otherwise a posix error.
""".
-spec recover(pcm(), error()) -> ok | {error, error()}.
recover(PCM, Error) ->
    recover_nif(PCM, Error, true).

%% nif
recover_nif(_PCM, _Error, _Silent) ->
    erlang:nif_error(not_loaded).


%% Reset the PCM position. This function resets the buffer and thus delay
%%      to 0 while keeping the playback running. This will most propably result
%%      in an underrun.
-doc false.
-spec reset(pcm()) -> ok | {error, error()}.
reset(PCM) ->
    reset_nif(PCM).

%% nif
reset_nif(_PCM) ->
    erlang:nif_error(not_loaded).


%% Resume from suspend, no samples are lost. This function can be used when
%%      the stream is in the suspend state to do the fine resume from this
%%      state. Not all hardware supports this feature, when an enosys error is
%%      returned, use the @{link prepare/1} function to recover.
%%
%%          as timeout, this function will return `{wait, {ready_resume, Reference}}',
%%          at which point, the caller should wait for the `{ready_resume, Reference}'
%%          message to be delivered to its message box before calling this
%%          function again.
-doc false.
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


-doc """
Starts the PCM. Normally you do not need to call this function for playback if
using `set_params/2`, as the auto-start mechanism will kick in. However, you
need to call this function in capture mode.

  * `PCM` - the PCM handle

Returns `ok` on success otherwise a posix error.
""".
-spec start(pcm()) -> ok | {error, error()}.
start(PCM) ->
    start_nif(PCM).

%% nif
start_nif(_PCM) ->
    erlang:nif_error(not_loaded).


-doc """
Writes interleaved audio frames to the PCM.

  * `PCM` - the PCM handle
  * `Buffer` - the audio data, which should hold a whole number of frames, see
    `frame_size/1`
  * `Timeout` - how long to wait, in milliseconds, for the device to accept
    the data, `infinity` to wait without a limit, or `nowait` to not wait

Returns:

  * `ok` - all the frames were written
  * `{wait, {ready_writei, Reference}, Remaining}` - only with `nowait`, when
    the device cannot accept more data yet. Wait for the
    `{ready_writei, Reference}` message to be delivered to the calling
    process, then call this function again with `Remaining`
  * `{error, incomplete_frame, Remaining}` - the whole frames of `Buffer` were
    written, and `Remaining` holds the trailing bytes that do not make a frame
  * `{error, timeout, Remaining}` - the device did not accept the data in time
  * `{error, Error, Remaining}` - a posix error. Use `recover/2` to recover
    from it. `Remaining` is the data that was not written

When `Buffer` ends with bytes that do not make a frame, those bytes are part of
`Remaining` in the `wait`, `timeout` and posix error results.
""".
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


-doc """
Reads interleaved audio frames from the PCM. At most 4096 frames are read at a
time.

  * `PCM` - the PCM handle
  * `Timeout` - how long to wait, in milliseconds, for the device to have
    data, `infinity` to wait without a limit, or `nowait` to not wait

Returns:

  * `{ok, Data}` - the audio data that was read, as interleaved frames
  * `{wait, {ready_readi, Reference}}` - only with `nowait`, when the device
    has no data yet. Wait for the `{ready_readi, Reference}` message to be
    delivered to the calling process, then call this function again
  * `{error, timeout}` - the device did not have data in time
  * `{error, Error}` - a posix error. Use `recover/2` to recover from it

In capture mode the PCM has to be started with `start/1` first.
""".
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


-doc """
Gets the size in bytes of a frame, that is of one sample for each channel, with
the current hardware parameters.

  * `PCM` - the PCM handle
""".
-spec frame_size(pcm()) -> pos_integer().
frame_size(PCM) ->
    frames_to_bytes_nif(PCM, 1).

%% nif
frames_to_bytes_nif(_PCM, _Frames) ->
    erlang:nif_error(not_loaded).


-doc """
Gets the size in bytes of a sample with the current hardware parameters.

  * `PCM` - the PCM handle
""".
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
    filename:join([Dir, "alsa_pcm_nif"]).
