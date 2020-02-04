# alsa

Erlang Alsa (libasound2) bindings.

The Alsa application allows for capture and playing audio through the Linux Alsa
interface.

# Setup

You need to add `alsa` as a dependency to your project. If you are using `rebar3`,
you can add the following to your `rebar.config`:

```erlang
{deps, [
    {alsa, "0.2.0"}
]}.
```

Also ensure that `alsa` is added as a dependency to your application, by updating
your `.app.src` file:

```erlang
{application, my_app, [

    {applications, [
        kernel,
        stdlib,

        alsa  % <- You need this in your applications list
    ]}
]}.
```

# Usage

For usage please refer to the examples in the examples directory. You can also
test the examples directly from the shell:

```erlang
1> c("examples/alsa_example_pcm.erl").
{ok,alsa_example_pcm}
2> alsa_example_pcm:play("default", alsa_example_pcm:generate_noise(5000)).
ok
```