# alsa

Erlang alsa (libasound2) bindings

## Build

    $ rebar3 compile

## Example

    $ rebar3 shell

    Then type the following into the shell:

    ```erlang
    alsa_output:main("default").
    ```

    You should hear random audio coming from the default soundcard.