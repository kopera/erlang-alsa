#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdatomic.h>

#include <assert.h>

#include <erl_nif.h>
#include <alsa/asoundlib.h>

#include "khash.h"

#define ARRAY_LENGTH(x) \
    ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))

/* Types */

KHASH_SET_INIT_INT(fd_set);

typedef struct alsa_pcm_nif_resource_t {
    ErlNifPid               owner;
    ErlNifMonitor           owner_monitor;

    snd_pcm_t              *handle;
    atomic_flag             handle_closed;

    khash_t(fd_set)        *select_fds;
} alsa_pcm_nif_resource_t;


/* Atoms */

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_wait;

static ERL_NIF_TERM am_undefined;

static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;

static ERL_NIF_TERM am_closed;

static ERL_NIF_TERM am_eagain;
static ERL_NIF_TERM am_ebadfd;
static ERL_NIF_TERM am_eintr;
static ERL_NIF_TERM am_enoent;
static ERL_NIF_TERM am_enomem;
static ERL_NIF_TERM am_enosys;
static ERL_NIF_TERM am_epipe;
static ERL_NIF_TERM am_estrpipe;

static ERL_NIF_TERM am_rw_interleaved;
static ERL_NIF_TERM am_rw_noninterleaved;

static ERL_NIF_TERM am_avail_min;
static ERL_NIF_TERM am_boundary;
static ERL_NIF_TERM am_start_threshold;
static ERL_NIF_TERM am_stop_threshold;
static ERL_NIF_TERM am_silence_threshold;
static ERL_NIF_TERM am_silence_size;

/* Helpers */

static ERL_NIF_TERM libasound_error_to_erl(ErlNifEnv *env, int error)
{
    switch (-error) {
        case EAGAIN: return am_eagain;
        case EBADFD: return am_ebadfd;
        case EINTR: return am_eintr;
        case ENOENT: return am_enoent;
        case ENOMEM: return am_enomem;
        case ENOSYS: return am_enosys;
        case EPIPE: return am_epipe;
        case ESTRPIPE: return am_estrpipe;
        default: return enif_make_int(env, -error);
    }
}

static bool alsa_pcm_nif_get_error(ErlNifEnv *env, const ERL_NIF_TERM term, int *value)
{
    if (enif_get_int(env, term, value)) {
        *value = -(*value);
        return true;
    }

    if (enif_is_atom(env, term)) {
        if (enif_is_identical(term, am_eagain)) {
            *value = -EAGAIN;
            return true;
        } else if (enif_is_identical(term, am_ebadfd)) {
            *value = -EBADFD;
            return true;
        } else if (enif_is_identical(term, am_eintr)) {
            *value = -EINTR;
            return true;
        } else if (enif_is_identical(term, am_enoent)) {
            *value = -ENOENT;
            return true;
        } else if (enif_is_identical(term, am_enomem)) {
            *value = -ENOMEM;
            return true;
        } else if (enif_is_identical(term, am_enosys)) {
            *value = -ENOSYS;
            return true;
        } else if (enif_is_identical(term, am_epipe)) {
            *value = -EPIPE;
            return true;
        } else if (enif_is_identical(term, am_estrpipe)) {
            *value = -ESTRPIPE;
            return true;
        }
    }

    return false;
}

static bool alsa_pcm_nif_get_bool(ErlNifEnv *env, const ERL_NIF_TERM term, bool *value)
{
    if (enif_is_identical(term, am_true)) {
        *value = true;
        return true;
    } else if (enif_is_identical(term, am_false)) {
        *value = false;
        return true;
    }

    return false;
}

static bool alsa_pcm_nif_get_pcm_access(ErlNifEnv *env, const ERL_NIF_TERM term, snd_pcm_access_t *value)
{
    if (enif_is_identical(term, am_rw_interleaved)) {
        *value = SND_PCM_ACCESS_RW_INTERLEAVED;
        return true;
    } else if (enif_is_identical(term, am_rw_noninterleaved)) {
        *value = SND_PCM_ACCESS_RW_NONINTERLEAVED;
        return true;
    }

    return false;
}

static bool alsa_pcm_nif_get_pcm_format(ErlNifEnv *env, const ERL_NIF_TERM term, snd_pcm_format_t *value)
{
    char format_name[256];
    if (!enif_get_atom(env, term, format_name, ARRAY_LENGTH(format_name), ERL_NIF_LATIN1)) {
        return false;
    }

    snd_pcm_format_t format = snd_pcm_format_value(format_name);
    if (format == SND_PCM_FORMAT_UNKNOWN) {
        return false;
    }

    *value = format;
    return true;
}

static bool alsa_pcm_nif_select(ErlNifEnv *env, alsa_pcm_nif_resource_t *resource, ERL_NIF_TERM msg)
{
    int poll_fds_count = snd_pcm_poll_descriptors_count(resource->handle);
    if (poll_fds_count <= 0) {
        return false;
    }

    struct pollfd *poll_fds = enif_alloc(poll_fds_count * sizeof(struct pollfd));
    poll_fds_count = snd_pcm_poll_descriptors(resource->handle, poll_fds, poll_fds_count);
    if (poll_fds_count <= 0) {
        enif_free (poll_fds);
        return false;
    }

    for (int i = 0; i < poll_fds_count; i++) {
        struct pollfd poll_fd = poll_fds[i];
        int mode = 0;
        if (poll_fd.events & POLLIN) {
            mode = ERL_NIF_SELECT_READ;
        }
        if (poll_fd.events & POLLOUT) {
            mode |= ERL_NIF_SELECT_WRITE;
        }
        assert (enif_select_x(env, poll_fd.fd, mode | ERL_NIF_SELECT_CUSTOM_MSG, resource, NULL, msg, NULL) >= 0);

        int kh_put_ret;
        kh_put(fd_set, resource->select_fds, poll_fd.fd, &kh_put_ret);
        assert (kh_put_ret >= 0);
    }
    enif_free(poll_fds);

    return true;
}

static bool alsa_pcm_nif_select_stop(ErlNifEnv *env, alsa_pcm_nif_resource_t *resource)
{
    khash_t(fd_set) *select_fds = resource->select_fds;
    khint_t select_fds_count = kh_size(select_fds);

    if (select_fds_count > 0) {
        int fds[select_fds_count];
        int fds_index = 0;

        for (khint_t i = kh_begin(select_fds); i != kh_end(select_fds); ++i) {
            if (!kh_exist(select_fds, i)) continue;

            fds[fds_index++] = kh_key(select_fds, i);
        }

        for (size_t i = 0; i < select_fds_count; i++) {
            assert (enif_select(env, fds[i], ERL_NIF_SELECT_STOP, resource, NULL, am_undefined) >= 0);
        }

        return true;
    }
    return false;
}


/* Resources */

/* Resource: pcm */
static ErlNifResourceType* alsa_pcm_nif_resource_type;

static alsa_pcm_nif_resource_t* alsa_pcm_nif_resource_new(ErlNifPid owner, snd_pcm_t *handle)
{
    alsa_pcm_nif_resource_t *resource = (alsa_pcm_nif_resource_t*) enif_alloc_resource(
        alsa_pcm_nif_resource_type,
        sizeof(alsa_pcm_nif_resource_t));
    resource->owner = owner;
    resource->handle = handle;
    atomic_flag_clear(&(resource->handle_closed));

    resource->select_fds = kh_init(fd_set);

    return resource;
}

static void alsa_pcm_nif_resource_dtor(ErlNifEnv *env, void *obj)
{
    alsa_pcm_nif_resource_t *resource = (alsa_pcm_nif_resource_t *) obj;

    kh_destroy(fd_set, resource->select_fds);
}

static void alsa_pcm_nif_resource_stop(ErlNifEnv *env, void *obj, int fd, int is_direct_call)
{
    alsa_pcm_nif_resource_t *resource = (alsa_pcm_nif_resource_t *) obj;

    khint_t iter = kh_get(fd_set, resource->select_fds, fd);
    kh_del(fd_set, resource->select_fds, iter);

    if (kh_size(resource->select_fds) == 0) {
        snd_pcm_close(resource->handle);
        resource->handle = NULL;
    }
}

static void alsa_pcm_nif_resource_owner_down(ErlNifEnv *env, void *obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    alsa_pcm_nif_resource_t *resource = (alsa_pcm_nif_resource_t *) obj;

    if (!atomic_flag_test_and_set(&resource->handle_closed)) {
        if (!alsa_pcm_nif_select_stop(env, resource)) {
            snd_pcm_close(resource->handle);
            resource->handle = NULL;
        }
    }
}

static ErlNifResourceTypeInit alsa_pcm_nif_resource_callbacks = {
    .dtor = alsa_pcm_nif_resource_dtor,
    .stop = alsa_pcm_nif_resource_stop,
    .down = alsa_pcm_nif_resource_owner_down,
};


/* API */

static ERL_NIF_TERM alsa_pcm_nif_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;

    char device[1024];
    if (!enif_get_string(env, argv[0], device, ARRAY_LENGTH(device), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    unsigned int dir;
    if (!enif_get_uint(env, argv[1], &dir)) {
        return enif_make_badarg(env);
    }
    snd_pcm_stream_t direction = dir == 0
        ? SND_PCM_STREAM_PLAYBACK
        : SND_PCM_STREAM_CAPTURE;

    snd_pcm_t *handle;
    ret = snd_pcm_open(&handle, device, direction, SND_PCM_NONBLOCK);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    alsa_pcm_nif_resource_t *resource = alsa_pcm_nif_resource_new(owner, handle);
    if (enif_monitor_process(env, resource, &owner, &resource->owner_monitor) != 0) {
        enif_release_resource(resource);
        snd_pcm_close(handle);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM result = enif_make_resource(env, resource);
    enif_release_resource(resource);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM alsa_pcm_nif_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    if (!atomic_flag_test_and_set(&resource->handle_closed)) {
        if (!alsa_pcm_nif_select_stop(env, resource)) {
            snd_pcm_close(resource->handle);
            resource->handle = NULL;
        }
        enif_demonitor_process(env, resource, &resource->owner_monitor);

        return am_ok;
    } else {
        return enif_make_tuple2(env, am_error, am_closed);
    }
}

static ERL_NIF_TERM alsa_pcm_nif_set_hwparams(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM params = argv[1];
    if (!enif_is_list(env, params)) {
        return enif_make_badarg(env);
    }

    snd_pcm_hw_params_t *hw_params;
    snd_pcm_hw_params_alloca(&hw_params);
    int ret = snd_pcm_hw_params_any(resource->handle, hw_params);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    ERL_NIF_TERM param;
    while (enif_get_list_cell(env, params, &param, &params)) {
        int arity;
        const ERL_NIF_TERM* fields;
        if (!enif_get_tuple(env, param, &arity, &fields) || arity != 2) {
            return enif_make_badarg(env);
        }

        char name[256];
        if (!enif_get_atom(env, fields[0], name, ARRAY_LENGTH(name), ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }

        if (strcmp("access", name) == 0) {
            snd_pcm_access_t access;
            if (!alsa_pcm_nif_get_pcm_access(env, fields[1], &access)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_hw_params_set_access(resource->handle, hw_params, access);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (strcmp("format", name) == 0) {
            snd_pcm_format_t format;
            if (!alsa_pcm_nif_get_pcm_format(env, fields[1], &format)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_hw_params_set_format(resource->handle, hw_params, format);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (strcmp("channels", name) == 0) {
            unsigned int channels;
            if (!enif_get_uint(env, fields[1], &channels)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_hw_params_set_channels(resource->handle, hw_params, channels);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (strcmp("rate", name) == 0) {
            unsigned int rate;
            if (!enif_get_uint(env, fields[1], &rate)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_hw_params_set_rate(resource->handle, hw_params, rate, 0);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (strcmp("rate_resample", name) == 0) {
            bool resample;
            if (!alsa_pcm_nif_get_bool(env, fields[1], &resample)) {
                return enif_make_badarg(env);
            }
            ret = snd_pcm_hw_params_set_rate_resample(resource->handle, hw_params, resample);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    if ((ret = snd_pcm_hw_params(resource->handle, hw_params)) < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_set_swparams(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM params = argv[1];
    if (!enif_is_list(env, params)) {
        return enif_make_badarg(env);
    }

    snd_pcm_sw_params_t *sw_params;
    snd_pcm_sw_params_alloca(&sw_params);
    int ret = snd_pcm_sw_params_current(resource->handle, sw_params);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    ERL_NIF_TERM param;
    while (enif_get_list_cell(env, params, &param, &params)) {
        int arity;
        const ERL_NIF_TERM* fields;
        if (!enif_get_tuple(env, param, &arity, &fields) || arity != 2) {
            return enif_make_badarg(env);
        }

        ERL_NIF_TERM name = fields[0];
        // if (!enif_get_atom(env, fields[0], name, ARRAY_LENGTH(name), ERL_NIF_LATIN1)) {
        //     return enif_make_badarg(env);
        // }

        if (enif_is_identical(name, am_avail_min)) {
            snd_pcm_uframes_t avail_min;
            if (!enif_get_ulong(env, fields[1], &avail_min)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_sw_params_set_avail_min(resource->handle, sw_params, avail_min);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (enif_is_identical(name, am_start_threshold)) {
            snd_pcm_uframes_t start_threshold;
            if (!enif_get_ulong(env, fields[1], &start_threshold)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_sw_params_set_start_threshold(resource->handle, sw_params, start_threshold);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (enif_is_identical(name, am_stop_threshold)) {
            snd_pcm_uframes_t stop_threshold;
            if (!enif_get_ulong(env, fields[1], &stop_threshold)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_sw_params_set_stop_threshold(resource->handle, sw_params, stop_threshold);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (enif_is_identical(name, am_silence_threshold)) {
            snd_pcm_uframes_t silence_threshold;
            if (!enif_get_ulong(env, fields[1], &silence_threshold)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_sw_params_set_silence_threshold(resource->handle, sw_params, silence_threshold);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else if (enif_is_identical(name, am_silence_size)) {
            snd_pcm_uframes_t silence_size;
            if (!enif_get_ulong(env, fields[1], &silence_size)) {
                return enif_make_badarg(env);
            }

            ret = snd_pcm_sw_params_set_silence_size(resource->handle, sw_params, silence_size);
            if (ret < 0) {
                return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    ret = snd_pcm_sw_params(resource->handle, sw_params);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_get_swparams(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    snd_pcm_sw_params_t *sw_params;
    snd_pcm_sw_params_alloca(&sw_params);
    int ret = snd_pcm_sw_params_current(resource->handle, sw_params);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_pcm_uframes_t avail_min;
    ret = snd_pcm_sw_params_get_avail_min(sw_params, &avail_min);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_pcm_uframes_t boundary;
    ret = snd_pcm_sw_params_get_boundary(sw_params, &boundary);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_pcm_uframes_t start_threshold;
    ret = snd_pcm_sw_params_get_start_threshold(sw_params, &start_threshold);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_pcm_uframes_t stop_threshold;
    ret = snd_pcm_sw_params_get_stop_threshold(sw_params, &stop_threshold);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_pcm_uframes_t silence_threshold;
    ret = snd_pcm_sw_params_get_silence_threshold(sw_params, &silence_threshold);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_pcm_uframes_t silence_size;
    ret = snd_pcm_sw_params_get_silence_size(sw_params, &silence_size);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    ERL_NIF_TERM keys[] = {
        am_avail_min,
        am_boundary,
        am_start_threshold,
        am_stop_threshold,
        am_silence_threshold,
        am_silence_size,
    };
    ERL_NIF_TERM values[] = {
        enif_make_ulong(env, avail_min),
        enif_make_ulong(env, boundary),
        enif_make_ulong(env, start_threshold),
        enif_make_ulong(env, stop_threshold),
        enif_make_ulong(env, silence_threshold),
        enif_make_ulong(env, silence_size),
    };

    ERL_NIF_TERM result;
    static_assert(ARRAY_LENGTH(keys) == ARRAY_LENGTH(values), "key/value size mismatch");
    enif_make_map_from_arrays(env, keys, values, ARRAY_LENGTH(keys), &result);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM alsa_pcm_nif_set_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    snd_pcm_format_t format;
    if (!alsa_pcm_nif_get_pcm_format(env, argv[1], &format)) {
        return enif_make_badarg(env);
    }

    snd_pcm_access_t access;
    if (!alsa_pcm_nif_get_pcm_access(env, argv[2], &access)) {
        return enif_make_badarg(env);
    }

    unsigned int channels;
    if (!enif_get_uint(env, argv[3], &channels)) {
        return enif_make_badarg(env);
    }

    unsigned int rate;
    if (!enif_get_uint(env, argv[4], &rate)) {
        return enif_make_badarg(env);
    }

    bool resample;
    if (!alsa_pcm_nif_get_bool(env, argv[5], &resample)) {
        return enif_make_badarg(env);
    }

    unsigned int latency;
    if (!enif_get_uint(env, argv[6], &latency)) {
        return enif_make_badarg(env);
    }

    int ret = snd_pcm_set_params(resource->handle, format, access, channels, rate, resample, latency);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_prepare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    int ret = snd_pcm_prepare(resource->handle);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_drop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    int ret = snd_pcm_drop(resource->handle);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_pause(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    bool pause;
    if (!alsa_pcm_nif_get_bool(env, argv[1], &pause)) {
        return enif_make_badarg(env);
    }

    int ret = snd_pcm_pause(resource->handle, pause);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_recover(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    int err;
    if (!alsa_pcm_nif_get_error(env, argv[1], &err)) {
        return enif_make_badarg(env);
    }

    bool silent;
    if (!alsa_pcm_nif_get_bool(env, argv[2], &silent)) {
        return enif_make_badarg(env);
    }

    int ret = snd_pcm_recover(resource->handle, err, silent);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    int ret = snd_pcm_reset(resource->handle) ;
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_resume(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM msg = argv[1];

    int ret = snd_pcm_resume(resource->handle);
    if (ret == -EAGAIN) {
        if (!alsa_pcm_nif_select(env, resource, msg)) {
            return enif_make_tuple2(env, am_error, am_eagain);
        }
        return am_wait;
    }
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    int ret = snd_pcm_start(resource->handle) ;
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }
    return am_ok;
}

static ERL_NIF_TERM alsa_pcm_nif_writei(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary buffer;
    if (!enif_inspect_binary(env, argv[1], &buffer)) {
        return enif_make_badarg(env);
    }

    snd_pcm_uframes_t frames;
    if (!enif_get_ulong(env, argv[2], &frames)) {
        return enif_make_badarg(env);
    }

    if (snd_pcm_frames_to_bytes(resource->handle, frames) != buffer.size) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM msg = argv[3];

    snd_pcm_sframes_t frames_written = snd_pcm_writei(resource->handle, buffer.data, frames);
    if (frames_written > 0) {
        return enif_make_tuple2(env, am_ok, enif_make_ulong(env, frames_written));
    } else if (frames_written == -EAGAIN || frames_written == 0) {
        if (!alsa_pcm_nif_select(env, resource, msg)) {
            return enif_make_tuple2(env, am_error, am_eagain);
        }
        return am_wait;
    } else {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, frames_written));
    }
}

static ERL_NIF_TERM alsa_pcm_nif_readi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    snd_pcm_uframes_t frames;
    if (!enif_get_ulong(env, argv[1], &frames)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM msg = argv[2];

    ErlNifBinary buffer;
    if (!enif_alloc_binary((size_t) snd_pcm_frames_to_bytes(resource->handle, frames), &buffer)) {
        return enif_make_tuple2(env, am_error, am_enomem);
    }

    snd_pcm_sframes_t frames_read = snd_pcm_readi(resource->handle, buffer.data, frames);
    if (frames_read > 0) {
        if ((size_t)frames_read < frames) {
            ssize_t bytes_read = snd_pcm_frames_to_bytes(resource->handle, frames_read);
            if (!enif_realloc_binary(&buffer, bytes_read)) {
                enif_release_binary(&buffer);
                return enif_make_tuple2(env, am_error, am_enomem);
            }
        }
        return enif_make_tuple3(env, am_ok,
            enif_make_binary(env, &buffer),
            enif_make_ulong(env, frames_read));
    } else if (frames_read == -EAGAIN || frames_read == 0) {
        enif_release_binary(&buffer);
        if (!alsa_pcm_nif_select(env, resource, msg)) {
            return enif_make_tuple2(env, am_error, am_eagain);
        }
        return am_wait;
    } else {
        enif_release_binary(&buffer);
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, frames_read));
    }
}

static ERL_NIF_TERM alsa_pcm_nif_frames_to_bytes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    snd_pcm_sframes_t frames;
    if (!enif_get_long(env, argv[1], &frames)) {
        return enif_make_badarg(env);
    }

    ssize_t bytes = snd_pcm_frames_to_bytes(resource->handle, frames) ;
    return enif_make_long(env, bytes);
}

static ERL_NIF_TERM alsa_pcm_nif_samples_to_bytes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_pcm_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_pcm_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    long samples;
    if (!enif_get_long(env, argv[1], &samples)) {
        return enif_make_badarg(env);
    }

    ssize_t bytes = snd_pcm_samples_to_bytes(resource->handle, samples) ;
    return enif_make_long(env, bytes);
}


/* Initialization */

static int alsa_pcm_nif_on_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    // Atoms
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_wait = enif_make_atom(env, "wait");

    am_undefined = enif_make_atom(env, "undefined");

    am_true = enif_make_atom(env, "true");
    am_false = enif_make_atom(env, "false");

    am_closed = enif_make_atom(env, "closed");

    am_eagain = enif_make_atom(env, "eagain");
    am_ebadfd = enif_make_atom(env, "ebadfd");
    am_eintr = enif_make_atom(env, "eintr");
    am_enoent = enif_make_atom(env, "enoent");
    am_enomem = enif_make_atom(env, "enomem");
    am_enosys = enif_make_atom(env, "enosys");
    am_epipe = enif_make_atom(env, "epipe");
    am_estrpipe = enif_make_atom(env, "estrpipe");

    am_rw_interleaved = enif_make_atom(env, "rw_interleaved");
    am_rw_noninterleaved = enif_make_atom(env, "rw_noninterleaved");

    am_avail_min = enif_make_atom(env, "avail_min");
    am_boundary = enif_make_atom(env, "boundary");
    am_start_threshold = enif_make_atom(env, "start_threshold");
    am_stop_threshold = enif_make_atom(env, "stop_threshold");
    am_silence_threshold = enif_make_atom(env, "silence_threshold");
    am_silence_size = enif_make_atom(env, "silence_size");

    // Resources
    alsa_pcm_nif_resource_type = enif_open_resource_type_x(env,
        "pcm",
        &alsa_pcm_nif_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    *priv_data = NULL;

    return 0;
}

static void alsa_pcm_nif_on_unload(ErlNifEnv *env, void* priv_data)
{
}


static int alsa_pcm_nif_on_upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (alsa_pcm_nif_on_load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}


static ErlNifFunc nif_funcs[] = {
    {"open_nif", 2, alsa_pcm_nif_open, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_nif", 1, alsa_pcm_nif_close},
    {"set_hwparams_nif", 2, alsa_pcm_nif_set_hwparams},
    {"set_swparams_nif", 2, alsa_pcm_nif_set_swparams},
    {"get_swparams_nif", 1, alsa_pcm_nif_get_swparams},
    {"set_params_nif", 7, alsa_pcm_nif_set_params},
    {"prepare_nif", 1, alsa_pcm_nif_prepare},
    {"drop_nif", 1, alsa_pcm_nif_drop},
    {"pause_nif", 2, alsa_pcm_nif_pause},
    {"recover_nif", 3, alsa_pcm_nif_recover},
    {"reset_nif", 1, alsa_pcm_nif_reset},
    {"resume_nif", 2, alsa_pcm_nif_resume},
    {"start_nif", 1, alsa_pcm_nif_start},
    {"writei_nif", 4, alsa_pcm_nif_writei},
    {"readi_nif", 3, alsa_pcm_nif_readi},

    {"frames_to_bytes_nif", 2, alsa_pcm_nif_frames_to_bytes},
    {"samples_to_bytes_nif", 2, alsa_pcm_nif_samples_to_bytes},
};

ERL_NIF_INIT(alsa_pcm, nif_funcs, alsa_pcm_nif_on_load, NULL, alsa_pcm_nif_on_upgrade, alsa_pcm_nif_on_unload);