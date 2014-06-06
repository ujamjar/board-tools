module Bram_16k_i : interface clk_clk end

module Sdram_i : interface clock_50_clk reset_reset_n end
module Sdram_o  : interface
    clock_100_sdram_clk pll_locked_export
    pll_phasedone_export clock_100_clk
    sdram_addr sdram_ba sdram_cas_n
    sdram_cke sdram_cs_n sdram_dqm
    sdram_ras_n sdram_we_n
end
module Sdram_t : interface sdram_dq end

module Ssram_i : interface clk_clk rst_reset_n altpll_0_areset_conduit_export end
module Ssram_o : interface ssram_tcm_address_out ssram_tcm_byteenable_n_out
    ssram_tcm_read_n_out ssram_tcm_write_n_out ssram_tcm_chipselect_n_out 
    altpll_0_locked_conduit_export altpll_0_phasedone_conduit_export
    ssram_clk_clk core_clk_clk end
module Ssram_t : interface ssram_tcm_data_out end

