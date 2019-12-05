-module(alsa_output).
-export([
    main/1
]).


main(Device) ->
    application:ensure_all_started(crypto),
    Buffer = crypto:strong_rand_bytes(16 * 1024),
    {ok, PCM} = alsa_pcm:open(Device, playback),
    ok = alsa_pcm:set_params(PCM, #{
        format => u8,
        access => rw_interleaved,
        channels => 1,
        rate => 48000,
        rate_resample => true,
        latency => 100000
    }),
    run(PCM, Buffer, 8).


run(PCM, Buffer, N) when N > 0 ->
    case alsa_pcm:writei(PCM, Buffer, byte_size(Buffer), infinity) of
        ok ->
            run(PCM, Buffer, N - 1);
        {error, Error} ->
            io:format("writei failure: ~p (trying recovery)~n", [Error]),
            case alsa_pcm:recover(PCM, Error) of
                ok ->
                    run(PCM, Buffer, N - 1);
                {error, Error} ->
                    io:format("writei failed: ~p~n", [Error]),
                    run(PCM, Buffer, 0)
            end
    end;

run(PCM, _Buffer, 0) ->
    alsa_pcm:close(PCM).

