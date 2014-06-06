module Bram_16k_i = interface clk_clk[1] end

module Sdram_i = interface clock_50_clk[1] reset_reset_n[1] end
module Sdram_o = interface  
    clock_100_sdram_clk[1] pll_locked_export[1]
    pll_phasedone_export[1] clock_100_clk[1]
    sdram_addr[13] sdram_ba[2] sdram_cas_n[1]
    sdram_cke[1] sdram_cs_n[1] sdram_dqm[1]
    sdram_ras_n[1] sdram_we_n[1]
end
module Sdram_t = interface sdram_dq end

module Ssram_i = interface clk_clk[1] rst_reset_n[1] altpll_0_areset_conduit_export[1] end
module Ssram_o = interface ssram_tcm_address_out[21] 
    ssram_tcm_byteenable_n_out[2] ssram_tcm_read_n_out[1] 
    ssram_tcm_write_n_out[1] ssram_tcm_chipselect_n_out[1] 
    altpll_0_locked_conduit_export[1] altpll_0_phasedone_conduit_export[1]
    ssram_clk_clk core_clk_clk[1] end
module Ssram_t = interface ssram_tcm_data_out[16] end

