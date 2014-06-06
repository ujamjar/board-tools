(* interfaces *)
module Main_i = interface clkin_125MHz[1](*n*) clkin_50MHz[1] cpu_rstN[1] pb[2] end
module Main_o = interface led[4] end

module Fsml_o = interface fsml_addr[24] fsml_oeN[1] fsml_weN[1] 
                    lcd_csN[1] max2_csN[1] flash_ceN[1] ssram_beN[2] 
                    ssram_ceN[1] ssram_clk[1] end
module Fsml_t = interface fsml_dq[16] end

module Enet_i = interface refclk_enet[1](*n*) enet_intN[1] enet_rx[1](*n*) end
module Enet_o = interface enet_rstN[1] enet_mdc[1] enet_tx[1](*n*) end
module Enet_t = interface enet_mdio[1] end

module Pcie_i = interface pcie_perstN[1] pcie_rx[1](*n*) refclk_pcie[1](*n*) end
module Pcie_o = interface pcie_tx[1](*n*) end

module Epcs_i = interface epcs_miso[1] end
module Epcs_o = interface epcs_clk[1] epcs_csN[1] epcs_mosi[1] end

module type Comb_S = HardCaml.Comb.S with type t = HardCaml.Structural.signal

let enet_lvds = [ "enet_tx"; "enet_rx" ]
let pcie_lvds = [ "pcie_tx"; "pcie_rx" ]


module Gen(C : Comb_S)(X : sig val filter : string list end) = struct
    open HardCaml.Structural
    open C

    let app f (a,b) = f a b 
    let app f (a,b) = 
        if List.mem a X.filter then Empty
        else f a b

    let inputs () = 
        let main_i = Main_i.(map (app mk_input) t) in
        let enet_i = Enet_i.(map (app mk_input) t) in
        let pcie_i = Pcie_i.(map (app mk_input) t) in
        let epcs_i = Epcs_i.(map (app mk_input) t) in
        main_i, enet_i, pcie_i, epcs_i

    let outputs () = 
        let main_o = Main_o.(map (app mk_output) t) in
        let enet_o = Enet_o.(map (app mk_output) t) in
        let pcie_o = Pcie_o.(map (app mk_output) t) in
        let epcs_o = Epcs_o.(map (app mk_output) t) in
        let fsml_o = Fsml_o.(map (app mk_output) t) in
        main_o, enet_o, pcie_o, epcs_o, fsml_o

    let tristates () = 
        let enet_t = Enet_t.(map (app mk_tristate) t) in
        let fsml_t = Fsml_t.(map (app mk_tristate) t) in
        enet_t, fsml_t

    
    let def0 (_,s) = zero s 
    let def1 (_,s) = ones s 
    let defz (_,s) = constz s 

    let def_outputs () = 
        let main_o' = Main_o.(map def0 t) in
        let enet_o' = Enet_o.({ map defz t with enet_rstn = gnd }) in
        let pcie_o' = Pcie_o.(map def0 t) in
        let epcs_o' = Epcs_o.(map defz t) in
        let fsml_o' = Fsml_o.({ map defz t with fsml_addr = consti 24 0; 
                                                lcd_csn = vdd; }) in
        main_o', enet_o', pcie_o', epcs_o', fsml_o'

    let def_tristates () = 
        let enet_t' = Enet_t.(map defz t) in
        let fsml_t' = Fsml_t.(map def0 t) in
        enet_t', fsml_t'

end

let port' (n,_) w = n, w 

module FilterAllLvds = struct
    let filter = enet_lvds @ pcie_lvds
end

let c4gx_15_gen name = 
    let open HardCaml.Structural in
    let _ = circuit name in
    let module Comb = HardCaml.Comb.Make(Base0) in
    let open Comb in
    let module Gen = Gen(Comb)(FilterAllLvds) in
    let open Gen in

    (* module interface *)
    let main_i, enet_i, pcie_i, epcs_i = inputs () in
    let main_o, enet_o, pcie_o, epcs_o, fsml_o = outputs () in
    let enet_t, fsml_t = tristates () in
    
    ignore (main_i,enet_i,pcie_i,epcs_i); (* avoid warnings *)

    (* instantiate the nios2 *)
    let () = 
        match name with
        | "c4gx_15_nios2_16k" -> begin
            inst "nios2_16k_lite"
                ~i:Altera_nios2.Bram_16k_i.(to_list (map2 port' t 
                    { clk_clk = main_i.Main_i.clkin_50mhz }))
        end
        | "c4gx_15_nios2_ssram" -> begin
            let addr = wire 21 in
            let core_clk = wire 1 in
            let pll_locked = wire 1 in
            let pll_phasedone = wire 1 in
            inst "nios2_ssram"
                ~i:Altera_nios2.Ssram_i.(to_list (map2 port' t 
                    { 
                        clk_clk = main_i.Main_i.clkin_50mhz;
                        altpll_0_areset_conduit_export = gnd;
                        rst_reset_n = vdd;
                    }))
                ~o:Altera_nios2.Ssram_o.(to_list (map2 port' t
                    {
                        ssram_tcm_address_out = addr;
                        ssram_tcm_byteenable_n_out = fsml_o.Fsml_o.ssram_ben;
                        ssram_tcm_read_n_out = fsml_o.Fsml_o.fsml_oen;
                        ssram_tcm_write_n_out = fsml_o.Fsml_o.fsml_wen;
                        ssram_tcm_chipselect_n_out = fsml_o.Fsml_o.ssram_cen;
                        ssram_clk_clk = fsml_o.Fsml_o.ssram_clk;
                        core_clk_clk = core_clk;
                        altpll_0_locked_conduit_export = pll_locked;
                        altpll_0_phasedone_conduit_export = pll_phasedone;
                    }))
                ~t:Altera_nios2.Ssram_t.(to_list (map2 port' t 
                    { ssram_tcm_data_out = fsml_t.Fsml_t.fsml_dq }));
            fsml_o.Fsml_o.fsml_addr <== uresize addr 24;
        end
        | _ -> ()

    in

    (* default outputs *)
    let main_o', enet_o', pcie_o', epcs_o', fsml_o' = def_outputs () in

    (* drive outputs if unassigned *)
    let conn o p = if not (is_connected o) then o <== p in
    let _ = Main_o.(map2 conn main_o main_o') in
    let _ = Enet_o.(map2 conn enet_o enet_o') in
    let _ = Pcie_o.(map2 conn pcie_o pcie_o') in
    let _ = Epcs_o.(map2 conn epcs_o epcs_o') in
    let _ = Fsml_o.(map2 conn fsml_o fsml_o') in

    (* default tristates *)
    let enet_t', fsml_t' = def_tristates () in

    (* drive tristates if unassigned *)
    let _ = Enet_t.(map2 conn enet_t enet_t') in
    let _ = Fsml_t.(map2 conn fsml_t fsml_t') in

    end_circuit()


let () = c4gx_15_gen "c4gx_15" 
let () = c4gx_15_gen "c4gx_15_nios2_16k" 
let () = c4gx_15_gen "c4gx_15_nios2_ssram" 

let write_top cname fname = 
    let open HardCaml.Structural in
    let circuit = find_circuit cname in
    let f = open_out fname in
    let () = write_verilog (output_string f) circuit in
    let () = close_out f in
    Lwt.return ()

type board_config = string * string

let c4gx_15 = "c4gx_15", ""
let c4gx_15_nios2_16k = "c4gx_15_nios2_16k", "nios2_16k_lite"
let c4gx_15_nios2_ssram = "c4gx_15_nios2_ssram", "nios2_ssram"

let board_settings (top_name,qsys_name) = 
    let open Altera_qsf in
    let path = try Sys.getenv "UJAMJAR_DATA" with _ -> failwith "set UJAMJAR_DATA" in
    let qsf_file = Filename.concat path "c4gxsk.qsf" in
    lwt () = Lwt_io.printf "opening %s\n" qsf_file in
    lwt qsf = parse_stream qsf_file in
    let clocks = [
        `DerivePll; `DeriveUncertainty;
        `CreateClock("clkin_50MHz", 20.0); `SetGroup("clkin_50MHz");
        `CreateClock("refclk_pcie", 10.0); `SetGroup("refclk_pcie");
        `CreateClock("clkin_125MHz", 8.0); `SetGroup("clkin_125MHz");
        (* XXX false paths *)
    ] in
    let files = [
        `Verilog, `Generate (write_top top_name), top_name ^ ".v";
        `Sdc, `Generate (Altera_quartus.Sdc.write_sdc clocks), top_name ^ ".sdc";
    ] in
    let files = 
        if qsys_name = "" then files 
        else
            (`Qsys, `Copy, Filename.concat path (qsys_name ^ ".qsys")) :: files
    in
    Lwt.return {
        part = "EP4CGX15BF14C6";
        top_level_entity = top_name;
        default_io_std = 
            (try get_global "STRATIX_DEVICE_IO_STANDARD" qsf
            with Not_found -> "2.5 V");
        reserve_after_config = get_reserve_after_config qsf;
        pins = get_pin_info qsf;
        files = files;
    }

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
        lwt () = Altera_quartus.Project.set_global_assignment 
            ~name:"RESERVE_ALL_UNUSED_PINS_WEAK_PULLUP" 
            "\"AS INPUT TRI-STATED WITH WEAK PULL-UP\"" p
        in
        lwt () = Altera_quartus.Project.set_global_assignment 
            ~name:"STRATIX_JTAG_USER_CODE"
            "DEADBEEF" p
        in
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


