(* de0-nano board *)

module Dram_o : interface
    DRAM_ADDR DRAM_BA DRAM_CAS_N 
    DRAM_CKE DRAM_CLK DRAM_CS_N
    DRAM_DQM DRAM_RAS_N DRAM_WE_N
end
module Dram_t : interface DRAM_DQ end

module Epcs_i : interface EPCS_DATA0 end
module Epcs_o : interface EPCS_ASDO EPCS_DCLK EPCS_NCSO end

module Acc_ee_i : interface G_SENSOR_INT end
module Acc_ee_o : interface G_SENSOR_CS_N I2C_SCLK end
module Acc_ee_t : interface I2C_SDAT end

module Adc_i : interface ADC_SDAT end
module Adc_o : interface ADC_CS_N ADC_SADDR ADC_SCLK end

module Gpio2x13_i : interface GPIO_2_IN end
module Gpio2x13_t : interface GPIO_2 end

module Gpio0_i : interface GPIO_0_IN end
module Gpio0_t : interface GPIO_0 end

module Gpio1_i : interface GPIO_1_IN end
module Gpio1_t : interface GPIO_1 end

module Main_i : interface CLOCK_50 KEY SW end
module Main_o : interface LED end

module Memif_i : interface memif_write memif_read memif_data_in end
module Memif_o : interface memif_data_out end

module Top_i : interface
    (epcs : Epcs_i)
    (acc_ee : Acc_ee_i)
    (gpio2x13 : Gpio2x13_i)
    (gpio0 : Gpio0_i)
    (gpio1 : Gpio1_i)
    (main : Main_i)
end
module Top_o : interface
    (dram : Dram_o)
    (epcs : Epcs_o)
    (acc_ee : Acc_ee_o)
    (main : Main_o)
end
module Top_t : interface
    (dram : Dram_t)
    (acc_ee : Acc_ee_t)
    (gpio2x13 : Gpio2x13_t)
    (gpio0 : Gpio0_t)
    (gpio1 : Gpio1_t)
end

type board_config = string * string
val nios2_16k : board_config
val nios2_sdram : board_config

val board_settings : board_config-> Altera_qsf.board_settings Lwt.t
val generate_board : ?force:bool -> string -> board_config -> 
    Altera_quartus.Report.resources Lwt.t



