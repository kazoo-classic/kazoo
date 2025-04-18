ROOT = ../..
PROJECT = kazoo_proper

ERLC_OPTS = +'{lager_extra_sinks, [data]}'

MP3 = priv/mp3.mp3

all: compile assets

assets: $(MP3)

$(MP3):
	@mkdir -p $(shell dirname $(MP3))
	@if [ -f $@ ]; then \
		echo "$@ already exists. Skipping download."; \
	else \
		echo "Downloading $@..."; \
		wget -qO $@ https://raw.githubusercontent.com/mathiasbynens/small/master/mp3.mp3 || { echo "Failed to fetch $@"; exit 1; }; \
	fi

include $(ROOT)/make/kz.mk
