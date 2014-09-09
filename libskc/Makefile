#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

# Targets
TARGETS := $(notdir ${SOURCE_DIR}).a

# Source files required to build the target
CFILES := $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/*.c) \
    $(wildcard $(SOURCE_DIR)/src/arch/${ARCH}/*.c))
ASMFILES := $(patsubst $(SOURCE_DIR)/%,%,$(wildcard ${SOURCE_DIR}/src/arch/$(ARCH)/*.S))

# Header files/directories this library provides
HDRFILES := $(wildcard $(SOURCE_DIR)/include/*)

# override the stage include dir so our headers won't conflict with those of
# other libcs
SEL4_INCLUDEDIR=$(STAGE_DIR)/include-$(notdir ${SOURCE_DIR})

include $(SEL4_COMMON)/common.mk
