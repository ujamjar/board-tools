hardcaml-board-tools
====================

An OCaml interface to the backend FPGA design tools and support for 
specific development boards.

Currently only the Altera tool chain is supported.  Xilinx support is planned
in the not too distant future.

The package consists of;

* An OCaml API which controls the Altera tools through quartus_sh
* routines to read/write various configuration/settings files
* Some basic board descriptions

