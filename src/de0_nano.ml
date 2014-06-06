(* de0-nano board *)

open HardCaml
open Signal.Comb

module Dram_o = interface
    DRAM_ADDR[13] DRAM_BA[2] DRAM_CAS_N[1] 
    DRAM_CKE[1] DRAM_CLK[1] DRAM_CS_N[1]
    DRAM_DQM[2] DRAM_RAS_N[1] DRAM_WE_N[1]
end
module Dram_t = interface DRAM_DQ[16] end

module Epcs_i = interface EPCS_DATA0[1] end
module Epcs_o = interface EPCS_ASDO[1] EPCS_DCLK[1] EPCS_NCSO[1] end

module Acc_ee_i = interface G_SENSOR_INT[1] end
module Acc_ee_o = interface G_SENSOR_CS_N[1] I2C_SCLK[1] end
module Acc_ee_t = interface I2C_SDAT[1] end

module Adc_i = interface ADC_SDAT[1] end
module Adc_o = interface ADC_CS_N[1] ADC_SADDR[1] ADC_SCLK[1] end

module Gpio2x13_i = interface GPIO_2_IN[3] end
module Gpio2x13_t = interface GPIO_2[13] end

module Gpio0_i = interface GPIO_0_IN[2] end
module Gpio0_t = interface GPIO_0[34] end

module Gpio1_i = interface GPIO_1_IN[2] end
module Gpio1_t = interface GPIO_1[34] end

module Main_i = interface CLOCK_50[1] KEY[2] SW[4] end
module Main_o = interface LED[8] end

module Memif_i = interface memif_write[1] memif_read[1] memif_data_in[32] end
module Memif_o = interface memif_data_out[32] end

module Top_i = interface
    (epcs : Epcs_i)
    (acc_ee : Acc_ee_i)
    (gpio2x13 : Gpio2x13_i)
    (gpio0 : Gpio0_i)
    (gpio1 : Gpio1_i)
    (main : Main_i)
end
module Top_o = interface
    (dram : Dram_o)
    (epcs : Epcs_o)
    (acc_ee : Acc_ee_o)
    (main : Main_o)
end
module Top_t = interface
    (dram : Dram_t)
    (acc_ee : Acc_ee_t)
    (gpio2x13 : Gpio2x13_t)
    (gpio0 : Gpio0_t)
    (gpio1 : Gpio1_t)
end

let port' (n,_) w = n, w 

let de0_nano_top = 
    let open HardCaml.Structural in
    
    let _ = circuit "de0_nano" in
    let module Comb = HardCaml.Comb.Make(Base0) in
    let open Comb in
    
    (* module interface *)
    let i = Top_i.(map (fun (n,b) -> mk_input n b) t) in
    let o = Top_o.(map (fun (n,b) -> mk_output n b) t) in
    let t = Top_t.(map (fun (n,b) -> mk_tristate n b) t) in

    (* Nios-II CPU *)
    let () = inst "nios2_16k"
        ~i:Altera_nios2.Bram_16k_i.(to_list (map2 port' t 
            { clk_clk = i.Top_i.main.Main_i.clock_50 }))
    in

    (* drive all outputs *)
    let _ = Top_o.map2 (fun (_,b) o -> o <== consti b 0) Top_o.t o in
    let _ = Top_t.map2 (fun (_,b) t -> t <== constz (width t)) Top_t.t t in
    end_circuit()

let de0_nano_sdram_top = 
    let open HardCaml.Structural in
    
    let _ = circuit "de0_nano_sdram" in
    let module Comb = HardCaml.Comb.Make(Base0) in
    let open Comb in
    
    (* module interface *)
    let i = Top_i.(map (fun (n,b) -> mk_input n b) t) in
    let o = Top_o.(map (fun (n,b) -> mk_output n b) t) in
    let t = Top_t.(map (fun (n,b) -> mk_tristate n b) t) in

    let nios_i = 
        let open Altera_nios2.Sdram_i in
        { 
            clock_50_clk = i.Top_i.main.Main_i.clock_50; 
            reset_reset_n = vdd;
        } 
    in

    let nios_o = 
        let open Altera_nios2.Sdram_o in
        let open Top_o in
        let open Dram_o in
        {
            sdram_addr = o.dram.dram_addr;
            sdram_ba = o.dram.dram_ba;
            sdram_cas_n = o.dram.dram_cas_n;
            sdram_cke = o.dram.dram_cke;
            sdram_cs_n = o.dram.dram_cs_n;
            sdram_dqm = o.dram.dram_dqm;
            sdram_ras_n = o.dram.dram_ras_n;
            sdram_we_n = o.dram.dram_we_n;
            clock_100_sdram_clk = o.dram.dram_clk;
            pll_locked_export = wire 1;
            pll_phasedone_export = wire 1;
            clock_100_clk = wire 1;
        }
    in

    let nios_t = 
        { 
            Altera_nios2.Sdram_t.sdram_dq = t.Top_t.dram.Dram_t.dram_dq;
        } 
    in

    (* Nios-II CPU *)
    let () = inst "nios2_sdram"
        ~i:Altera_nios2.Sdram_i.(to_list (map2 port' t nios_i))
        ~o:Altera_nios2.Sdram_o.(to_list (map2 port' t nios_o))
        ~t:Altera_nios2.Sdram_t.(to_list (map2 port' t nios_t))
    in

    (* drive unconnected outputs *)
    let def_o o = if not (is_connected o) then o <== consti (width o) 0 in
    let def_t t = if not (is_connected t) then t <== constz (width t) in
    let _ = Top_o.map def_o o in
    let _ = Top_t.map def_t t in
    end_circuit ()

let write_top cname fname = 
    let open HardCaml.Structural in
    let circuit = find_circuit cname in
    let f = open_out fname in
    let () = write_verilog (output_string f) circuit in
    let () = close_out f in
    Lwt.return ()

type board_config = string * string
let nios2_16k = "de0_nano", "nios2_16k"
let nios2_sdram = "de0_nano_sdram", "nios2_sdram"

let board_settings (top_name,qsys_name) = 
    let open Altera_qsf in
    let path = try Sys.getenv "UJAMJAR_DATA" with _ -> failwith "set UJAMJAR_DATA" in
    let qsf_file = Filename.concat path "de0_nano.qsf" in
    lwt () = Lwt_io.printf "opening %s\n" qsf_file in
    lwt qsf = parse_stream qsf_file in
    let clocks = [ `DerivePll; `CreateClock("CLOCK_50",20.0) ] in
    Lwt.return {
        part = "EP4CE22F17C6";
        top_level_entity = top_name;
        default_io_std = 
            (try get_global "STRATIX_DEVICE_IO_STANDARD" qsf
            with Not_found -> "3.3-V LVTTL");
        reserve_after_config = get_reserve_after_config qsf;
        pins = get_pin_info qsf;
        files = [
            `Verilog, `Generate (write_top top_name), top_name ^ ".v";
            `Sdc, `Generate (Altera_quartus.Sdc.write_sdc clocks), top_name ^ ".sdc"; 
            `Qsys, `Copy, Filename.concat path (qsys_name ^ ".qsys");
        ];
    }


(* generate and run a quartus project and produce a .sof *)
let generate_board ?(force=false) path cfg = 
    let open Altera_quartus in
    let open Altera_qsf in
    let _ = Utils.verbose := true in

    lwt () = Lwt_io.printf "getting board settings...\n" in
    lwt settings = board_settings cfg in
    let name = settings.top_level_entity in
    let full_path = Filename.concat path name in
    lwt () = Lwt_io.printf "building project %s...\n" full_path in
    new_project ~force full_path settings.part (fun p -> 
        (* configure project *)
        lwt () = apply_board_settings settings p in
        (* full compile flow *)
        lwt () = Misc.load_package "flow" p in
        lwt ok,err = Flow.execute_flow `Compile p in
        if ok then
            (* return usage report *)
            lwt _ = Misc.load_package "report" p in
            lwt _ = Report.load_report p in
            lwt report = Report.get_fitter_report p in
            lwt _ = Report.unload_report p in
            (*lwt _ = Utils.command_unit "exit\n" p in *)
            Lwt.return report
        else
            Lwt.fail (Failure "flow failed")
    )

