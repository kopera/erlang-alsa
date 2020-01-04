#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdatomic.h>

#include <assert.h>

#include <erl_nif.h>
#include <alsa/asoundlib.h>

#define ARRAY_LENGTH(x) \
    ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))

#define clamp(value, min, max) \
	((value < min) ? (min) : ((value > max) ? (max) : (value)))

/* Types */

typedef struct alsa_ctl_nif_resource_t {
    ErlNifPid               owner;
    ErlNifMonitor           owner_monitor;

    snd_ctl_t            *handle;
    atomic_flag             handle_closed;
} alsa_ctl_nif_resource_t;


/* Atoms */

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_wait;

static ERL_NIF_TERM am_undefined;

static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;

static ERL_NIF_TERM am_closed;
static ERL_NIF_TERM am_other;

static ERL_NIF_TERM am_enomem;
static ERL_NIF_TERM am_enotsup;
static ERL_NIF_TERM am_eagain;

static ERL_NIF_TERM am_card;
static ERL_NIF_TERM am_mixer;
static ERL_NIF_TERM am_pcm;
static ERL_NIF_TERM am_rawmidi;
static ERL_NIF_TERM am_timer;
static ERL_NIF_TERM am_sequencer;
static ERL_NIF_TERM am_numid;
static ERL_NIF_TERM am_interface;
static ERL_NIF_TERM am_name;
static ERL_NIF_TERM am_index;
static ERL_NIF_TERM am_device;
static ERL_NIF_TERM am_subdevice;


/* Helpers */

static ERL_NIF_TERM libasound_error_to_erl(ErlNifEnv *env, int error)
{
    switch (-error) {
        case ENOMEM: return am_enomem;
        default: return enif_make_int(env, -error);
    }
}

static bool alsa_ctl_nif_get_bool(ErlNifEnv *env, const ERL_NIF_TERM term, bool *value)
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

static bool alsa_ctl_nif_get_elem_iface(ErlNifEnv *env, const ERL_NIF_TERM term, snd_ctl_elem_iface_t *interface)
{
    if (enif_is_identical(term, am_card)) {
        *interface = SND_CTL_ELEM_IFACE_CARD;
        return true;
    } else if (enif_is_identical(term, am_mixer)) {
        *interface = SND_CTL_ELEM_IFACE_MIXER;
        return true;
    } else if (enif_is_identical(term, am_pcm)) {
        *interface = SND_CTL_ELEM_IFACE_PCM;
        return true;
    } else if (enif_is_identical(term, am_rawmidi)) {
        *interface = SND_CTL_ELEM_IFACE_RAWMIDI;
        return true;
    } else if (enif_is_identical(term, am_timer)) {
        *interface = SND_CTL_ELEM_IFACE_TIMER;
        return true;
    } else if (enif_is_identical(term, am_sequencer)) {
        *interface = SND_CTL_ELEM_IFACE_SEQUENCER;
        return true;
    } else {
        return false;
    }
}

static bool alsa_ctl_nif_get_ctl_elem_id(ErlNifEnv *env, const ERL_NIF_TERM term, snd_ctl_elem_id_t *elem_id)
{
    ErlNifMapIterator iter;
    if (enif_map_iterator_create(env, term, &iter, ERL_NIF_MAP_ITERATOR_FIRST)) {
        ERL_NIF_TERM key, value;

        snd_ctl_elem_id_set_interface(elem_id, SND_CTL_ELEM_IFACE_MIXER);
        while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
            if (enif_is_identical(key, am_numid)) {
                unsigned int numid;
                if (!enif_get_uint(env, value, &numid)) {
                    return false;
                }
                snd_ctl_elem_id_set_numid(elem_id, numid);
            } else if (enif_is_identical(key, am_interface)) {
                snd_ctl_elem_iface_t interface;
                if (!alsa_ctl_nif_get_elem_iface(env, value, &interface)) {
                    return false;
                }
                snd_ctl_elem_id_set_interface(elem_id, interface);
            } else if (enif_is_identical(key, am_name)) {
                char name[64];
                if (!enif_get_string(env, value, name, ARRAY_LENGTH(name), ERL_NIF_LATIN1)) {
                    return false;
                }
                snd_ctl_elem_id_set_name(elem_id, name);
            } else if (enif_is_identical(key, am_index)) {
                unsigned int index;
                if (!enif_get_uint(env, value, &index)) {
                    return false;
                }
                snd_ctl_elem_id_set_index(elem_id, index);
            } else if (enif_is_identical(key, am_device)) {
                unsigned int device;
                if (!enif_get_uint(env, value, &device)) {
                    return false;
                }
                snd_ctl_elem_id_set_device(elem_id, device);
            } else if (enif_is_identical(key, am_subdevice)) {
                unsigned int subdevice;
                if (!enif_get_uint(env, value, &subdevice)) {
                    return false;
                }
                snd_ctl_elem_id_set_subdevice(elem_id, subdevice);
            } else {
                return false;
            }

            enif_map_iterator_next(env, &iter);
        }

        enif_map_iterator_destroy(env, &iter);
        return true;
    }

    return false;
}

static bool alsa_ctl_nif_get_ctl_elem_value_enumerated(ErlNifEnv *env, const ERL_NIF_TERM term, snd_ctl_t *handle, snd_ctl_elem_info_t *elem_info, unsigned int* elem_value)
{
    char value[1024];
    if (!enif_get_string(env, term, value, sizeof(value), ERL_NIF_LATIN1)) {
        return false;
    }

    unsigned int items = snd_ctl_elem_info_get_items(elem_info);
    unsigned int item;
    int ret;
    for (item = 0; item < items; item++) {
        snd_ctl_elem_info_set_item(elem_info, item);
        ret = snd_ctl_elem_info(handle, elem_info);
        if (ret >= 0 && strcmp(value, snd_ctl_elem_info_get_item_name(elem_info)) == 0) {
            *elem_value = item;
            return true;
        }
    }

    return false;
}

static bool alsa_ctl_nif_get_ctl_elem_value(ErlNifEnv *env, const ERL_NIF_TERM term, snd_ctl_t *handle, snd_ctl_elem_info_t *elem_info, snd_ctl_elem_value_t *elem_value)
{
    snd_ctl_elem_type_t type = snd_ctl_elem_info_get_type(elem_info);
    unsigned int count = snd_ctl_elem_info_get_count(elem_info);
    unsigned int i;

    if (type == SND_CTL_ELEM_TYPE_BYTES) {
        ErlNifBinary bin;
        if (!enif_inspect_binary(env, term, &bin) || bin.size != count) {
            return false;
        }
        for (i = 0; i < count; i++) {
            snd_ctl_elem_value_set_byte(elem_value, i, bin.data[i]);
        }
        return true;
    }

    int arity;
    const ERL_NIF_TERM* values;
    if (!enif_get_tuple(env, term, &arity, &values) || arity != count) {
        return false;
    }

    for (i = 0; i < count; i++) {
        switch (type) {
            case SND_CTL_ELEM_TYPE_BOOLEAN: {
                bool value;
                if (!alsa_ctl_nif_get_bool(env, values[i], &value)) {
                    return false;
                }
                snd_ctl_elem_value_set_boolean(elem_value, i, value ? 1 : 0);
                break;
            }
            case SND_CTL_ELEM_TYPE_INTEGER: {
                long value;
                if (!enif_get_long(env, values[i], &value)) {
                    return false;
                }

                long min = snd_ctl_elem_info_get_min(elem_info);
                long max = snd_ctl_elem_info_get_max(elem_info);
                snd_ctl_elem_value_set_integer(elem_value, i, clamp(value, min, max));
                break;
            }
            case SND_CTL_ELEM_TYPE_INTEGER64: {
                int64_t value;
                if (!enif_get_int64(env, values[i], &value)) {
                    return false;
                }

                long long min = snd_ctl_elem_info_get_min64(elem_info);
                long long max = snd_ctl_elem_info_get_max64(elem_info);
                snd_ctl_elem_value_set_integer64(elem_value, i, clamp(value, min, max));
                break;
            }
            case SND_CTL_ELEM_TYPE_ENUMERATED: {
                unsigned int value;
                if (!alsa_ctl_nif_get_ctl_elem_value_enumerated(env, values[i], handle, elem_info, &value)) {
                    return false;
                }
                snd_ctl_elem_value_set_enumerated(elem_value, i, value);
                break;
            }
            default:
                return false;
        }
    }

    return true;
}

static ERL_NIF_TERM alsa_ctl_nif_make_elem_iface(snd_ctl_elem_iface_t interface)
{
    switch (interface) {
        case SND_CTL_ELEM_IFACE_CARD:
            return am_card;
        case SND_CTL_ELEM_IFACE_MIXER:
            return am_mixer;
        case SND_CTL_ELEM_IFACE_PCM:
            return am_pcm;
        case SND_CTL_ELEM_IFACE_RAWMIDI:
            return am_rawmidi;
        case SND_CTL_ELEM_IFACE_TIMER:
            return am_timer;
        case SND_CTL_ELEM_IFACE_SEQUENCER:
            return am_sequencer;
        default:
            return am_other;
    }
}

static ERL_NIF_TERM alsa_ctl_nif_make_ctl_elem_id(ErlNifEnv *env, const snd_ctl_elem_id_t *elem_id)
{
    ERL_NIF_TERM keys[] = {
        am_numid,
        am_interface,
        am_name,
        am_index,
        am_device,
        am_subdevice,
    };
    ERL_NIF_TERM values[] = {
        enif_make_uint(env, snd_ctl_elem_id_get_numid(elem_id)),
        alsa_ctl_nif_make_elem_iface(snd_ctl_elem_id_get_interface(elem_id)),
        enif_make_string(env, snd_ctl_elem_id_get_name(elem_id), ERL_NIF_LATIN1),
        enif_make_uint(env, snd_ctl_elem_id_get_index(elem_id)),
        enif_make_uint(env, snd_ctl_elem_id_get_device(elem_id)),
        enif_make_uint(env, snd_ctl_elem_id_get_subdevice(elem_id)),
    };

    ERL_NIF_TERM result;
    static_assert(ARRAY_LENGTH(keys) == ARRAY_LENGTH(values), "key/value size mismatch");
    enif_make_map_from_arrays(env, keys, values, ARRAY_LENGTH(keys), &result);

    return result;
}


static ERL_NIF_TERM alsa_ctl_nif_make_ctl_elem_value(ErlNifEnv *env, snd_ctl_t *handle, snd_ctl_elem_info_t *elem_info, snd_ctl_elem_value_t *elem_value)
{
    snd_ctl_elem_type_t type = snd_ctl_elem_info_get_type(elem_info);
    unsigned int count = snd_ctl_elem_info_get_count(elem_info);
    unsigned int i;

    if (type == SND_CTL_ELEM_TYPE_BYTES) {
        ERL_NIF_TERM result;
        unsigned char* result_buffer = enif_make_new_binary(env, count, &result);
        if (result_buffer != NULL) {
            memcpy(result_buffer, snd_ctl_elem_value_get_bytes(elem_value), count);
            return result;
        } else {
            return enif_make_tuple2(env, am_error, am_enomem);
        }
    }

    ERL_NIF_TERM values[count];
    for (i = 0; i < count; i++) {
        switch (type) {
            case SND_CTL_ELEM_TYPE_BOOLEAN: {
                values[i] = snd_ctl_elem_value_get_boolean(elem_value, i)
                    ? am_true
                    : am_false;
                break;
            }
            case SND_CTL_ELEM_TYPE_INTEGER: {
                values[i] = enif_make_long(env, snd_ctl_elem_value_get_integer(elem_value, i));
                break;
            }
            case SND_CTL_ELEM_TYPE_INTEGER64: {
                values[i] = enif_make_int64(env, snd_ctl_elem_value_get_integer64(elem_value, i));
                break;
            }
            case SND_CTL_ELEM_TYPE_ENUMERATED: {
                unsigned int item = snd_ctl_elem_value_get_enumerated(elem_value, i);
                int ret;

                snd_ctl_elem_info_set_item(elem_info, item);
                ret = snd_ctl_elem_info(handle, elem_info);
                if (ret < 0) {
                    return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
                }

                values[i] = enif_make_string(env, snd_ctl_elem_info_get_item_name(elem_info), ERL_NIF_LATIN1);
                break;
            }
            default:
                return enif_make_tuple2(env, am_error, am_enotsup);
        }
    }

    return enif_make_tuple_from_array(env, values, count);
}

/* Resources */

/* Resource: ctl */
static ErlNifResourceType* alsa_ctl_nif_resource_type;

static alsa_ctl_nif_resource_t* alsa_ctl_nif_resource_new(ErlNifPid owner, snd_ctl_t *handle)
{
    alsa_ctl_nif_resource_t *resource = (alsa_ctl_nif_resource_t*) enif_alloc_resource(
        alsa_ctl_nif_resource_type,
        sizeof(alsa_ctl_nif_resource_t));
    resource->owner = owner;
    resource->handle = handle;
    atomic_flag_clear(&(resource->handle_closed));

    // resource->select_fds = kh_init(fd_set);

    return resource;
}

// static void alsa_ctl_nif_resource_dtor(ErlNifEnv *env, void *obj)
// {
//     alsa_ctl_nif_resource_t *resource = (alsa_ctl_nif_resource_t *) obj;

//     kh_destroy(fd_set, resource->select_fds);
// }

// static void alsa_ctl_nif_resource_stop(ErlNifEnv *env, void *obj, int fd, int is_direct_call)
// {
//     alsa_ctl_nif_resource_t *resource = (alsa_ctl_nif_resource_t *) obj;

//     khint_t iter = kh_get(fd_set, resource->select_fds, fd);
//     kh_del(fd_set, resource->select_fds, iter);

//     if (kh_size(resource->select_fds) == 0) {
//         snd_ctl_close(resource->handle);
//         resource->handle = NULL;
//     }
// }

static void alsa_ctl_nif_resource_owner_down(ErlNifEnv *env, void *obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    alsa_ctl_nif_resource_t *resource = (alsa_ctl_nif_resource_t *) obj;

    if (!atomic_flag_test_and_set(&resource->handle_closed)) {
        snd_ctl_close(resource->handle);
        resource->handle = NULL;
    }
}

static ErlNifResourceTypeInit alsa_ctl_nif_resource_callbacks = {
    // .dtor = alsa_ctl_nif_resource_dtor,
    // .stop = alsa_ctl_nif_resource_stop,
    .down = alsa_ctl_nif_resource_owner_down,
};


/* API */

static ERL_NIF_TERM alsa_ctl_nif_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;

    char card[1024];
    if (!enif_get_string(env, argv[0], card, ARRAY_LENGTH(card), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    snd_ctl_t *handle;
    ret = snd_ctl_open(&handle, card, SND_CTL_NONBLOCK);
    if (ret != 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    alsa_ctl_nif_resource_t *resource = alsa_ctl_nif_resource_new(owner, handle);
    if (enif_monitor_process(env, resource, &owner, &resource->owner_monitor) != 0) {
        enif_release_resource(resource);
        snd_ctl_close(handle);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM result = enif_make_resource(env, resource);
    enif_release_resource(resource);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM alsa_ctl_nif_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alsa_ctl_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_ctl_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    if (!atomic_flag_test_and_set(&resource->handle_closed)) {
        snd_ctl_close(resource->handle);
        resource->handle = NULL;
        enif_demonitor_process(env, resource, &resource->owner_monitor);

        return am_ok;
    } else {
        return enif_make_tuple2(env, am_error, am_closed);
    }
}

// static ERL_NIF_TERM alsa_ctl_nif_read_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
// {
//     alsa_ctl_nif_resource_t *resource;
//     if (!enif_get_resource(env, argv[0], alsa_ctl_nif_resource_type, (void**) &resource)) {
//         return enif_make_badarg(env);
//     }

//     ERL_NIF_TERM msg = argv[1];

//     snd_ctl_event_t *event;
//     snd_ctl_event_alloca(&event);
//     int event_read = snd_ctl_read(resource->handle, event);
//     if (event_read > 0) {
//         return enif_make_tuple2(env, am_ok, alsa_ctl_nif_make_event(env, event));
//     } else if (event_read == -EAGAIN || event_read == -EWOULDBLOCK || event_read == 0) {
//         if (!alsa_ctl_nif_select(env, resource, msg)) {
//             return enif_make_tuple2(env, am_error, am_eagain);
//         }
//         return am_wait;
//     } else {
//         return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, event_read));
//     }
// }

static ERL_NIF_TERM alsa_ctl_nif_elem_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;

    alsa_ctl_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_ctl_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    snd_ctl_elem_list_t *elem_list;
    snd_ctl_elem_list_alloca(&elem_list);
    ret = snd_ctl_elem_list(resource->handle, elem_list);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    unsigned int elem_count = snd_ctl_elem_list_get_count(elem_list);

    snd_ctl_elem_list_set_offset(elem_list, 0);
    ret = snd_ctl_elem_list_alloc_space(elem_list, elem_count);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    ret = snd_ctl_elem_list(resource->handle, elem_list);
    if (ret < 0) {
        snd_ctl_elem_list_free_space(elem_list);
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    unsigned int i;
    ERL_NIF_TERM result = enif_make_list(env, 0);
    for (i = 0; i < elem_count; ++i) {
        snd_ctl_elem_id_t *elem_id;
        snd_ctl_elem_id_alloca(&elem_id);
        snd_ctl_elem_list_get_id(elem_list, i, elem_id);

        result = enif_make_list_cell(env,
            alsa_ctl_nif_make_ctl_elem_id(env, elem_id),
            result);
    }

    snd_ctl_elem_list_free_space(elem_list);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM alsa_ctl_nif_elem_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;

    alsa_ctl_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_ctl_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    snd_ctl_elem_id_t *elem_id;
    snd_ctl_elem_id_alloca(&elem_id);
    if (!alsa_ctl_nif_get_ctl_elem_id(env, argv[1], elem_id)) {
        return enif_make_badarg(env);
    }

    snd_ctl_elem_info_t *elem_info;
    snd_ctl_elem_info_alloca(&elem_info);
    snd_ctl_elem_info_set_id(elem_info, elem_id);
    ret = snd_ctl_elem_info(resource->handle, elem_info);
    if (ret != 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_ctl_elem_value_t *elem_value;
    snd_ctl_elem_value_alloca(&elem_value);
    snd_ctl_elem_value_set_id(elem_value, elem_id);
    ret = snd_ctl_elem_read(resource->handle, elem_value);
    if (ret != 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    return alsa_ctl_nif_make_ctl_elem_value(env, resource->handle, elem_info, elem_value);
}


static ERL_NIF_TERM alsa_ctl_nif_elem_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;

    alsa_ctl_nif_resource_t *resource;
    if (!enif_get_resource(env, argv[0], alsa_ctl_nif_resource_type, (void**) &resource)) {
        return enif_make_badarg(env);
    }

    snd_ctl_elem_id_t *elem_id;
    snd_ctl_elem_id_alloca(&elem_id);
    if (!alsa_ctl_nif_get_ctl_elem_id(env, argv[1], elem_id)) {
        return enif_make_badarg(env);
    }

    snd_ctl_elem_info_t *elem_info;
    snd_ctl_elem_info_alloca(&elem_info);
    snd_ctl_elem_info_set_id(elem_info, elem_id);
    ret = snd_ctl_elem_info(resource->handle, elem_info);
    if (ret != 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    snd_ctl_elem_value_t *elem_value;
    snd_ctl_elem_value_alloca(&elem_value);
    snd_ctl_elem_value_set_id(elem_value, elem_id);
    if (!alsa_ctl_nif_get_ctl_elem_value(env, argv[2], resource->handle, elem_info, elem_value)) {
        return enif_make_badarg(env);
    }

    ret = snd_ctl_elem_write(resource->handle, elem_value);
    if (ret != 0) {
        return enif_make_tuple2(env, am_error, libasound_error_to_erl(env, ret));
    }

    return am_ok;
}



/* Initialization */

static int alsa_ctl_nif_on_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    // Atoms
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_wait = enif_make_atom(env, "wait");

    am_undefined = enif_make_atom(env, "undefined");

    am_true = enif_make_atom(env, "true");
    am_false = enif_make_atom(env, "false");

    am_closed = enif_make_atom(env, "closed");
    am_other = enif_make_atom(env, "other");

    am_enomem = enif_make_atom(env, "enomem");
    am_enotsup = enif_make_atom(env, "enotsup");
    am_eagain = enif_make_atom(env, "eagain");

    am_card = enif_make_atom(env, "card");
    am_mixer = enif_make_atom(env, "mixer");
    am_pcm = enif_make_atom(env, "pcm");
    am_rawmidi = enif_make_atom(env, "rawmidi");
    am_timer = enif_make_atom(env, "timer");
    am_sequencer = enif_make_atom(env, "sequencer");
    am_numid = enif_make_atom(env, "numid");
    am_interface = enif_make_atom(env, "interface");
    am_name = enif_make_atom(env, "name");
    am_index = enif_make_atom(env, "index");
    am_device = enif_make_atom(env, "device");
    am_subdevice = enif_make_atom(env, "subdevice");

    // Resources
    alsa_ctl_nif_resource_type = enif_open_resource_type_x(env,
        "ctl",
        &alsa_ctl_nif_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    *priv_data = NULL;

    return 0;
}

static void alsa_ctl_nif_on_unload(ErlNifEnv *env, void* priv_data)
{
}


static int alsa_ctl_nif_on_upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (alsa_ctl_nif_on_load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}


static ErlNifFunc nif_funcs[] = {
    {"open_nif", 1, alsa_ctl_nif_open, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_nif", 1, alsa_ctl_nif_close},
    // {"read_event_nif", 2, alsa_ctl_nif_read_event},
    {"elem_list_nif", 1, alsa_ctl_nif_elem_list, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"elem_read_nif", 2, alsa_ctl_nif_elem_read, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"elem_write_nif", 3, alsa_ctl_nif_elem_write, ERL_NIF_DIRTY_JOB_IO_BOUND},
};

ERL_NIF_INIT(alsa_ctl, nif_funcs, alsa_ctl_nif_on_load, NULL, alsa_ctl_nif_on_upgrade, alsa_ctl_nif_on_unload);