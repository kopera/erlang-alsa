#include <assert.h>
#include <stdlib.h>

#include <ei.h>
#include <alsa/asoundlib.h>

#include "erlport.h"


static snd_mixer_t *mixer;
static erlport_t *port;

static void encode_list_kv_header(ei_x_buff *buffer, const char* key) {
    ei_x_encode_list_header(buffer, 1);
    ei_x_encode_tuple_header(buffer, 2);
    ei_x_encode_atom(buffer, key);
}

static void encode_alsa_mixer_element_playback(ei_x_buff *buffer, snd_mixer_elem_t *elem)
{
    bool has_volume = snd_mixer_selem_has_playback_volume(elem);
    bool has_switch = snd_mixer_selem_has_playback_switch(elem);

    if (has_volume) {
        long volume[SND_MIXER_SCHN_LAST + 1] = {0,};
        size_t volume_length = 0;

        long volume_db[SND_MIXER_SCHN_LAST + 1] = {0,};
        size_t volume_db_length = 0;

        for (int c = 0; c <= SND_MIXER_SCHN_LAST; c++) {
            if (snd_mixer_selem_has_playback_channel(elem, c)) {
                snd_mixer_selem_get_playback_volume(elem, c, &volume[volume_length]);
                volume_length++;

                if (snd_mixer_selem_get_playback_dB(elem, c, &volume_db[volume_db_length]) >= 0) {
                    volume_db_length++;
                }
            }
        }

        long range_min = 0, range_max = 0;
        snd_mixer_selem_get_playback_volume_range(elem, &range_min, &range_max);

        encode_list_kv_header(buffer, "range");
        ei_x_encode_tuple_header(buffer, 2);
        ei_x_encode_long(buffer, range_min);
        ei_x_encode_long(buffer, range_max);

        if (snd_mixer_selem_get_playback_dB_range(elem, &range_min, &range_max) >= 0) {
            encode_list_kv_header(buffer, "db_range");
            ei_x_encode_tuple_header(buffer, 2);
            ei_x_encode_double(buffer, range_min / 100.0);
            ei_x_encode_double(buffer, range_max / 100.0);
        }

        if (volume_length > 0) {
            encode_list_kv_header(buffer, "volume");
            ei_x_encode_tuple_header(buffer, volume_length);
            for (int i = 0; i < volume_length; i++) {
                ei_x_encode_long(buffer, volume[i]);
            }
        }
        if (volume_db_length > 0 && volume_db_length == volume_length) {
            encode_list_kv_header(buffer, "volume_db");
            ei_x_encode_tuple_header(buffer, volume_db_length);
            for (int i = 0; i < volume_db_length; i++) {
                ei_x_encode_double(buffer, volume_db[i] / 100.0);
            }
        }

        encode_list_kv_header(buffer, "volume_joined");
        ei_x_encode_boolean(buffer, snd_mixer_selem_has_playback_volume_joined(elem));
    }

    if (has_switch) {
        int switches[SND_MIXER_SCHN_LAST + 1] = {0,};
        size_t switches_length = 0;

        for (int c = 0; c <= SND_MIXER_SCHN_LAST; c++) {
            if (snd_mixer_selem_has_playback_channel(elem, c)) {
                snd_mixer_selem_get_playback_switch(elem, c, &switches[switches_length]);
                switches_length++;
            }
        }

        if (switches_length > 0) {
            encode_list_kv_header(buffer, "switch");
            ei_x_encode_tuple_header(buffer, switches_length);
            for (int i = 0; i < switches_length; i++) {
                ei_x_encode_atom(buffer, switches[i] ? "on" : "off");
            }

            encode_list_kv_header(buffer, "switch_joined");
            ei_x_encode_boolean(buffer, snd_mixer_selem_has_playback_switch_joined(elem));
        }
    }

    ei_x_encode_empty_list(buffer);
}

static void encode_alsa_mixer_element(ei_x_buff *buffer, snd_mixer_elem_t *elem)
{
    bool has_playback = snd_mixer_selem_has_playback_volume(elem) || snd_mixer_selem_has_playback_switch(elem);

    ei_x_encode_tuple_header(buffer, 2);

    /* ID */
    ei_x_encode_tuple_header(buffer, 2);
    ei_x_encode_string(buffer, snd_mixer_selem_get_name(elem));
    ei_x_encode_ulong(buffer, snd_mixer_selem_get_index(elem));

    long metadata_size = 0;
    if (has_playback) {
        metadata_size += 1;
    }

    /* Metadata */
    ei_x_encode_map_header(buffer, metadata_size);

    if (has_playback) {
        ei_x_encode_atom(buffer, "playback");
        encode_alsa_mixer_element_playback(buffer, elem);
    }
}

static void notify_alsa_mixer_event(const char *event, snd_mixer_elem_t *elem)
{
    ei_x_buff buffer;
    ei_x_new_with_version(&buffer);
    ei_x_encode_tuple_header(&buffer, 2);
        ei_x_encode_atom(&buffer, event);
        encode_alsa_mixer_element(&buffer, elem);
    erlport_send(port, buffer.buff, buffer.index);
    ei_x_free(&buffer);
}

static void notify_synchronizing()
{
    erlport_send(port, "", 0);
}

static void notify_synchronized()
{
    ei_x_buff buffer;
    ei_x_new_with_version(&buffer);
    ei_x_encode_atom(&buffer, "synchronized");
    erlport_send(port, buffer.buff, buffer.index);
    ei_x_free(&buffer);
}

static int mixer_elem_event(snd_mixer_elem_t *elem, unsigned int mask)
{
    if (mask == SND_CTL_EVENT_MASK_REMOVE) {
        notify_alsa_mixer_event("remove", elem);
        return 0;
    }

    if (mask & (SND_CTL_EVENT_MASK_INFO | SND_CTL_EVENT_MASK_TLV | SND_CTL_EVENT_MASK_VALUE)) {
        notify_alsa_mixer_event("update", elem);
    }

    return 0;
}

static int mixer_event(snd_mixer_t *mixer, unsigned int mask, snd_mixer_elem_t *elem)
{
    if (mask & SND_CTL_EVENT_MASK_ADD) {
        notify_alsa_mixer_event("add", elem);
        snd_mixer_elem_set_callback(elem, mixer_elem_event);
    }
    return 0;
}

static int erlport_decode_element_id(const char *message, int *message_index, char **name, unsigned long *index)
{
    int arity;
    if (ei_decode_tuple_header(message, message_index, &arity) < 0 || arity != 2) {
        return -1;
    }

    int type, size;
    ei_get_type(message, message_index, &type, &size);
    if (type != ERL_STRING_EXT) {
        return -1;
    }

    *name = calloc(size + 1, sizeof(char));
    if (ei_decode_string(message, message_index, *name) < 0) {
        free(*name);
        return -1;
    }

    if (ei_decode_ulong(message, message_index, index) < 0) {
        free(*name);
        return -1;
    }

    return 0;
}

static void erlport_reply_error(const erlport_t *port, unsigned long req_id, const char *error)
{
    ei_x_buff buffer;
    ei_x_new_with_version(&buffer);
    ei_x_encode_tuple_header(&buffer, 3);
        ei_x_encode_atom(&buffer, "reply");
        ei_x_encode_ulong(&buffer, req_id);
        ei_x_encode_tuple_header(&buffer, 2);
            ei_x_encode_atom(&buffer, "error");
            ei_x_encode_atom(&buffer, error);
    erlport_send(port, buffer.buff, buffer.index);
    ei_x_free(&buffer);
}

static void erlport_reply_ok0(const erlport_t *port, unsigned long req_id)
{
    ei_x_buff buffer;
    ei_x_new_with_version(&buffer);
    ei_x_encode_tuple_header(&buffer, 3);
        ei_x_encode_atom(&buffer, "reply");
        ei_x_encode_ulong(&buffer, req_id);
        ei_x_encode_atom(&buffer, "ok");
    erlport_send(port, buffer.buff, buffer.index);
    ei_x_free(&buffer);
}

static void erlport_handle_set_playback_volume_all(const erlport_t *port, unsigned long req_id, const char *message, int *message_index)
{
    snd_mixer_elem_t *elem;
    snd_mixer_selem_id_t *sid;

    int arity;
    if (ei_decode_tuple_header(message, message_index, &arity) < 0 || arity != 2) {
        erlport_reply_error(port, req_id, "malformed_request");
    }

    char* element_name;
    unsigned long element_index;
    if (erlport_decode_element_id(message, message_index, &element_name, &element_index) < 0) {
        erlport_reply_error(port, req_id, "malformed_request");
    }
    snd_mixer_selem_id_alloca(&sid);
    snd_mixer_selem_id_set_name(sid, element_name);
    snd_mixer_selem_id_set_index(sid, element_index);
    elem = snd_mixer_find_selem(mixer, sid);
    free(element_name);

    long volume;
    if (ei_decode_long(message, message_index, &volume) < 0) {
        erlport_reply_error(port, req_id, "malformed_request");
    }

    if (elem) {
        if (snd_mixer_selem_set_playback_volume_all(elem, volume) < 0) {
            erlport_reply_error(port, req_id, "bad_request");
        } else {
            erlport_reply_ok0(port, req_id);
            notify_alsa_mixer_event("update", elem);
        }
    } else {
        erlport_reply_error(port, req_id, "not_found");
    }
}

static void erlport_handle_set_playback_switch_all(const erlport_t *port, unsigned long req_id, const char *message, int *message_index)
{
    snd_mixer_elem_t *elem;
    snd_mixer_selem_id_t *sid;

    int arity;
    if (ei_decode_tuple_header(message, message_index, &arity) < 0 || arity != 2) {
        erlport_reply_error(port, req_id, "malformed_request");
    }

    char* element_name;
    unsigned long element_index;
    if (erlport_decode_element_id(message, message_index, &element_name, &element_index) < 0) {
        erlport_reply_error(port, req_id, "malformed_request");
    }
    snd_mixer_selem_id_alloca(&sid);
    snd_mixer_selem_id_set_name(sid, element_name);
    snd_mixer_selem_id_set_index(sid, element_index);
    elem = snd_mixer_find_selem(mixer, sid);
    free(element_name);

    char switch_value[MAXATOMLEN];
    if (ei_decode_atom(message, message_index, switch_value) < 0) {
        erlport_reply_error(port, req_id, "malformed_request");
    }

    if (elem) {
        if (snd_mixer_selem_set_playback_switch_all(elem, strcmp(switch_value, "on") == 0) < 0) {
            erlport_reply_error(port, req_id, "bad_request");
        } else {
            erlport_reply_ok0(port, req_id);
            notify_alsa_mixer_event("update", elem);
        }
    } else {
        erlport_reply_error(port, req_id, "not_found");
    }
}

struct erlport_event_handler {
    const char *request_tag;
    void (*handle)(const erlport_t *port, unsigned long req_id, const char *message, int *message_index);
};

static struct erlport_event_handler erlport_event_handlers[] = {
    { "set_playback_volume_all", erlport_handle_set_playback_volume_all },
    { "set_playback_switch_all", erlport_handle_set_playback_switch_all },
    { NULL, NULL }
};

static void erlport_event(const erlport_t *port, const void *message, size_t message_length, void *data)
{
    int message_index = 0;

    if (ei_decode_version(message, &message_index, NULL) < 0) {
        exit(65);
    }

    int arity;
    if (ei_decode_tuple_header(message, &message_index, &arity) < 0 || arity != 3) {
        exit(65);
    }

    unsigned long request_id;
    if (ei_decode_ulong(message, &message_index, &request_id) < 0) {
        exit(65);
    }

    char request_tag[MAXATOMLEN];
    if (ei_decode_atom(message, &message_index, request_tag) < 0) {
        exit(65);
    }

    for (struct erlport_event_handler *handler = erlport_event_handlers; handler->request_tag != NULL; handler++) {
        if (strcmp(request_tag, handler->request_tag) == 0) {
            handler->handle(port, request_id, message, &message_index);
            return;
        }
    }

    exit(65);
}


int main(int argc, char *argv[])
{
    int ret;
    if (argc > 2) {
        printf("Usage: %s [device]\n", argv[0]);
        exit(64);
    }
    const char* device = argc >= 2 ? argv[1] : "default";

    port = erlport_open(fileno(stdin), fileno(stdout), erlport_event, NULL);

    if ((ret = snd_mixer_open(&mixer, 0)) < 0) {
        exit(70);
    }

    if ((ret = snd_mixer_attach(mixer, device)) < 0) {
        exit(66);
    }

    notify_synchronizing();

    if ((ret = snd_mixer_selem_register(mixer, NULL, NULL)) < 0) {
        exit(70);
    }

    snd_mixer_set_callback(mixer, mixer_event);

    if ((ret = snd_mixer_load(mixer)) < 0) {
        exit(70);
    }

    notify_synchronized();

    struct pollfd *pollfds = NULL;
    size_t pollfds_length = 0;
    for (;;) {
        ret = snd_mixer_poll_descriptors_count(mixer) + 1;
        if (pollfds_length != ret) {
            free(pollfds);

            pollfds_length = ret;
            pollfds = calloc(pollfds_length, sizeof(struct pollfd));
        }

        if ((ret = erlport_get_poll_descriptors(port, &pollfds[0])) < 0) {
            exit(70);
        }

        if ((ret = snd_mixer_poll_descriptors(mixer, &pollfds[1], pollfds_length - 1)) < 0) {
            exit(70);
        }

        if ((ret = poll(pollfds, pollfds_length, -1)) < 0 && errno != EINTR) {
            exit(71);
        }

        if (pollfds[0].revents & (POLLERR | POLLHUP | POLLNVAL)) {
            break;
        }

        if (pollfds[0].revents & POLLIN) {
            erlport_handle_events(port);
        }

        {
            unsigned short revents = 0;
            if ((ret = snd_mixer_poll_descriptors_revents(mixer, &pollfds[1], pollfds_length - 1, &revents)) < 0) {
                exit(EXIT_FAILURE);
            }

            if (revents & (POLLERR | POLLNVAL)) {
                exit(EXIT_FAILURE);
            }

            if (revents & POLLIN) {
                ret = snd_mixer_handle_events(mixer);
                assert(ret >= 0);
            }
        }
    }
    free(pollfds);

    erlport_close(&port, false);
    snd_mixer_close(mixer);

    return 0;
}
