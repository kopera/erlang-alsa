CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/../..)

ERL ?= erl
PKG_CONFIG ?= pkg-config

UNAME_SYS := $(shell uname -s)

# System specific C compiler/flags.
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O2 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O2 -finline-functions -Wall -Wmissing-prototypes
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O2 -finline-functions -Wall -Wmissing-prototypes
endif

# Project specific C compiler/flags.
CFLAGS += -std=c99 -D_GNU_SOURCE -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
CFLAGS += $(shell pkg-config --cflags alsa)

LDFLAGS += -L $(ERL_INTERFACE_LIB_DIR) -lei
LDFLAGS += $(shell $(PKG_CONFIG) --libs alsa)

# Verbosity
V ?= 0

gen_verbose_0 = @echo " GEN   " $(@F);
gen_verbose = $(gen_verbose_$(V))

c_verbose_0 = @echo " C     " $(@F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

compile_c = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c

# Files
SOURCES := \
	alsa_mixer.c \
	erlport.c

HEADERS := $(wildcard *.h)
OBJECTS = $(addprefix _build/, $(addsuffix .o, $(basename $(SOURCES))))

# Targets
all: $(BASEDIR)/priv/alsa_mixer_port

clean:
	@rm -f "$(BASEDIR)/priv/alsa_mixer_port"
	@rm -rf _build/
	@rm -f env.mk

# Targets (internal)

$(BASEDIR)/priv/alsa_mixer_port: $(OBJECTS) | $(BASEDIR)/priv/
	$(link_verbose) $(CC) $^ $(LDFLAGS) -o "$@"

$(BASEDIR)/priv/:
	@mkdir -p "$@"

_build/%.o: %.c $(HEADERS) | _build/
	$(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o "$@"

_build/:
	@mkdir -p "$@"

env.mk: $(MAKEFILE_LIST)
	$(gen_verbose) $(ERL) -noshell -eval "file:write_file(\"$@\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		halt()."
-include env.mk
