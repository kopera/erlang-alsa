%% @hidden
-module(alsa_test).
-export([
    play/2,
    record/1,
    record/2
]).

-export([
    generate_noise/1
]).

play(Device, Data) ->
    {ok, PCM} = alsa_pcm:open(Device, playback),
    ok = alsa_pcm:set_params(PCM, #{
        format => s16_le,
        access => rw_interleaved,
        channels => 2,
        rate => 48000,
        rate_resample => true,
        latency => 100000
    }),
    play_i(PCM, Data),
    alsa_pcm:close(PCM).


play_i(PCM, Data) ->
    case alsa_pcm:writei(PCM, Data, infinity) of
        ok ->
            ok;
        {error, Error} ->
            io:format("writei failure: ~p (trying recovery)~n", [Error]),
            case alsa_pcm:recover(PCM, Error) of
                ok ->
                    play_i(PCM, Data);
                {error, Error} ->
                    exit(Error)
            end
    end.


record(Device) ->
    record(Device, 5000).

record(Device, Duration) ->
    {ok, PCM} = alsa_pcm:open(Device, capture),
    ok = alsa_pcm:set_params(PCM, #{
        format => s16_le,
        access => rw_interleaved,
        channels => 2,
        rate => 48000,
        rate_resample => true,
        latency => 100000
    }),
    Size = alsa_pcm:frame_size(PCM) * duration_to_frames(#{rate => 48000, channels => 2}, Duration),
    Data = record_i(PCM, Size, <<>>),
    alsa_pcm:close(PCM),
    Data.


record_i(PCM, Size, Acc) ->
    case alsa_pcm:readi(PCM, infinity) of
        {ok, Data} when byte_size(Data) >= Size ->
            <<Acc/binary, Data/binary>>;
        {ok, Data} ->
            record_i(PCM, Size - byte_size(Data), <<Acc/binary, Data/binary>>);
        {error, Error} ->
            io:format("writei failure: ~p (trying recovery)~n", [Error]),
            case alsa_pcm:recover(PCM, Error) of
                ok ->
                    record_i(PCM, Size, Acc);
                {error, Error} ->
                    exit(Error)
            end
    end.

duration_to_frames(#{rate := Rate, channels := Channels}, Duration) ->
    round((Duration * Rate / 1000) / Channels).


generate_noise(Duration) ->
    Channels = 2,
    Depth = 2,
    Frames = duration_to_frames(#{rate => 48000, channels => Channels}, Duration),
    application:ensure_all_started(crypto),
    crypto:strong_rand_bytes(Frames * Depth * Channels).