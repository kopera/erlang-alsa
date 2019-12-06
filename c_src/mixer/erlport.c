#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "erlport.h"


struct erlport {
    int input_fd;
    char input_buffer[(1 << 16) + sizeof(uint16_t)];
    size_t input_buffer_index;

    int output_fd;

    erlport_callback_t callback;
    void* callback_data;
};


struct erlport *erlport_open(int input_fd, int output_fd, erlport_callback_t callback, void* callback_data)
{
    struct erlport *port = calloc(1, sizeof(struct erlport));
    port->input_fd = input_fd;
    port->output_fd = output_fd;
    port->callback = callback;
    port->callback_data = callback_data;

    return port;
}

void erlport_close(struct erlport **port, bool close_fds)
{
    if (*port) {
        if (close_fds) {
            close((*port)->input_fd);
            close((*port)->output_fd);
        }

        free(*port);
        *port = NULL;
    }
}

int erlport_get_poll_descriptors(const struct erlport *port, struct pollfd pollfds[static 1])
{
    pollfds[0].fd = port->input_fd;
    pollfds[0].events = POLLIN;
    pollfds[0].revents = 0;

    return 1;
}

static size_t erlport_dispatch_event(struct erlport *port)
{
    if (port->input_buffer_index < sizeof(uint16_t)) {
        return 0;
    }

    uint8_t header[2];
    memcpy(header, port->input_buffer, sizeof(header));
    uint16_t length = (header[0] << 8) | header[1];

    if (length + sizeof(header) > port->input_buffer_index) {
        return 0;
    }

    port->callback(port, &port->input_buffer[sizeof(header)], length, port->callback_data);

    return length + sizeof(header);
}

int erlport_handle_events(struct erlport *port)
{
    ssize_t ret = read(port->input_fd,
                       &port->input_buffer[port->input_buffer_index],
                       sizeof(port->input_buffer) - port->input_buffer_index);

    if (ret < 0) {
        if (errno == EINTR) {
            return 0;
        }
        exit(74);
    } else if (ret == 0) {
        return 0;
    } else {
        port->input_buffer_index += ret;

        int count = 0;
        for (;;) {
            size_t consumed = erlport_dispatch_event(port);

            if (consumed == 0) {
                return count;
            } else if (port->input_buffer_index > consumed) {
                count += 1;
                memmove(port->input_buffer, &port->input_buffer[consumed], port->input_buffer_index - consumed);
                port->input_buffer_index -= consumed;
            } else {
                port->input_buffer_index = 0;
                return count + 1;
            }
        }
    }
}

static void write_exact(int fd, const void *message, size_t message_length)
{
    size_t bytes_written = 0;
    do {
        ssize_t ret = write(fd, &((const uint8_t *)message)[bytes_written], message_length - bytes_written);
        if (ret < 0) {
            if (errno == EINTR)
                continue;

            exit(74);
        }
        bytes_written += ret;
    } while (bytes_written < message_length);
}

void erlport_send(const struct erlport *port, const void *message, size_t message_length)
{
    char header[2] = {
        ((message_length >> 8) & 0xff),
        ((message_length >> 0) & 0xff),
    };
    write_exact(port->output_fd, header, sizeof(header));
    write_exact(port->output_fd, message, message_length);
}
