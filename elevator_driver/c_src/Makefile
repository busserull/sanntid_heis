# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))


C_SRC_OUTPUT ?= $(CURDIR)/../priv/elevator_driver

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	C_SRC_DIR = $(CURDIR)/../simulator
	CC ?= cc
	CFLAGS ?= -g -std=c99 -arch x86_64 -Wall -Wmissing-prototypes
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress

else ifeq ($(UNAME_SYS), Linux)
	C_SRC_DIR = $(CURDIR)
	CC ?= gcc
	CFLAGS ?= -g -std=gnu11 -Wall -Wmissing-prototypes
	LDFLAGS = -lcomedi -pthread -lm -lerl_interface -lei -lnsl
endif

SOURCES := $(shell find $(C_SRC_DIR) -name '*.c')
OBJECTS = $(SOURCES:%.c=%.o)

# program executable file name.
TARGET = $(C_SRC_OUTPUT)

# top-level rule, to compile everything.
all: $(SOURCES)

# rule to link the program
$(TARGET): $(OBJECTS)
	$(CC) -o $@ $^ $(LDFLAGS)

# Compile: create object files from C source files.
%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

# rule for cleaning re-compilable files.
clean:
	rm -f $(TARGET) $(OBJECTS)

rebuild:	clean all

.PHONY: all rebuild clean



