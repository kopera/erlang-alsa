# alsa

Erlang alsa (libasound2) bindings

## Build

    $ rebar3 compile

## Example

    $ rebar3 shell

    Then type the following into the shell:

    ```erlang
    alsa_test:play("default", alsa_test:generate_noise(5000)).
    ```

    You should hear random audio coming from the default soundcard.