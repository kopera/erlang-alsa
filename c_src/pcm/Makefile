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

LDFLAGS += -shared

# Project specific C compiler/flags.
ifeq ($(ERTS_INCLUDE_DIR),)
	ERTS_INCLUDE_DIR := $(shell "$(ERL)" -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
endif

CFLAGS += -std=gnu11 -fPIC -I $(ERTS_INCLUDE_DIR)
CFLAGS += $(shell $(PKG_CONFIG) --cflags alsa)

LDFLAGS += $(shell $(PKG_CONFIG) --libs alsa)

# Verbosity.
V ?= 0

c_verbose_0 = @echo " C     " $(@F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

# Files
SOURCES := \
	alsa_pcm_nif.c

HEADERS := $(wildcard *.h)
OBJECTS = $(addprefix _build/, $(addsuffix .o, $(basename $(SOURCES))))

all: $(BASEDIR)/priv/alsa_pcm_nif.so

$(BASEDIR)/priv/alsa_pcm_nif.so: $(OBJECTS) | $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) -o "$@"

$(BASEDIR)/priv/:
	@mkdir -p "$@"

_build/%.o: %.c $(HEADERS) | _build/
	$(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c "$<" -o "$@"

_build/:
	@mkdir -p "$@"

clean:
	@rm -f "$(BASEDIR)/priv/alsa_pcm_nif.so"
	@rm -Rf "_build/"