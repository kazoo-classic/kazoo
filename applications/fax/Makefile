ROOT = ../..
PROJECT = fax

PDF = test/pdf.pdf

COMPILE_MOAR = assets

all: compile assets

comile-test: assets

assets: $(PDF)

$(PDF):
	@if [ -f $@ ]; then \
		echo "$@ already exists. Skipping download."; \
	else \
		echo "Downloading $@..."; \
		wget -qO $@ https://raw.githubusercontent.com/mathiasbynens/small/master/pdf.pdf || { echo "Failed to fetch $@"; exit 1; }; \
	fi


include $(ROOT)/make/kz.mk
