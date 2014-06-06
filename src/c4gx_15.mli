module Main_i : interface clkin_125MHz(*n*) clkin_50MHz cpu_rstN pb end
module Main_o : interface led end

module Fsml_o : interface fsml_addr fsml_oeN fsml_weN 
                    lcd_csN max2_csN flash_ceN ssram_beN 
                    ssram_ceN ssram_clk end
module Fsml_t : interface fsml_dq end

module Enet_i : interface refclk_enet(*n*) enet_intN enet_rx(*n*) end
module Enet_o : interface enet_rstN enet_mdc enet_tx(*n*) end
module Enet_t : interface enet_mdio end

module Pcie_i : interface pcie_perstN pcie_rx(*n*) refclk_pcie(*n*) end
module Pcie_o : interface pcie_tx(*n*) end

module Epcs_i : interface epcs_miso end
module Epcs_o : interface epcs_clk epcs_csN epcs_mosi end

type board_config = string * string
val c4gx_15 : board_config
val c4gx_15_nios2_16k : board_config
val c4gx_15_nios2_ssram : board_config

val board_settings : board_config -> Altera_qsf.board_settings Lwt.t

val generate_board : ?force:bool -> string -> board_config -> 
    Altera_quartus.Report.resources Lwt.t

