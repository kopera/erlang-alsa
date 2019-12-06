#ifndef __ERLPORT_H__
#define __ERLPORT_H__

#include <stdbool.h>
#include <poll.h>

struct erlport;
typedef struct erlport erlport_t;

typedef void(* erlport_callback_t)(const erlport_t *port, const void *message, size_t message_length, void* callback_data);

erlport_t *erlport_open(int input_fd, int output_fd, erlport_callback_t callback, void* callback_data);
void erlport_close(erlport_t **port, bool close_fds);
int erlport_get_poll_descriptors(const erlport_t *port, struct pollfd pollfds[static 1]);
int erlport_handle_events(erlport_t *port);
void erlport_send(const erlport_t *port, const void *message, size_t message_length);

#endif /* __ERLPORT_H__ */
