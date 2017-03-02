ifndef QUARTUS_ROOTDIR
  $(error Please set QUARTUS_ROOTDIR)
endif

# Local compiler flags
CPPFLAGS = -I $(INC) -O

# Top-level dependencies
DEPS = Main.cpp JtagAtlantic.h RawLink.h

.PHONY: all
all: hostlink

hostlink: $(DEPS)
	g++ Main.cpp -o hostlink \
    $(CPPFLAGS) \
    -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/

run: hostlink
	quartus_pgm -m jtag -o "p;../de5/Golden_top.sof" \
   && LD_LIBRARY_PATH=$(QUARTUS_ROOTDIR)/linux64 time -p ./hostlink

.PHONY: clean
clean:
	rm -f *.o hostlink
