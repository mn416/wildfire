ifndef QUARTUS_ROOTDIR
  $(error Please set QUARTUS_ROOTDIR)
endif

.PHONY: all
all:
	qsys-generate -syn S5_DDR3_QSYS.qsys
	quartus_sh --flow compile Golden_top.qpf

.PHONY: download-sof
download-sof:
	quartus_pgm -m jtag -o "p;Golden_top.sof"

.PHONY: clean
clean:
	rm -f *.mif *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg
	rm -f *.log *.bak Solver.v
	rm -rf db incremental_db S5_DDR3_QSYS .qsys_edit dse
