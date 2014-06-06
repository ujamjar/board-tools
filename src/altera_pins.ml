(* description of altera fpga pins *)

(* read the fpga description files *)

(* requried fpga devices;

	Cyclone IV E EP4CE22 - Terasic DE0-Nano
	Cyclone IV GX EP4CGX15 - Terasic Transceiver Starter Kit
	Cyclone IV GX EP4CGX150 - Terasic Development Kit

*)

(***************************************************************************)
(* types *)

type side = Left | Bottom | Right | Top
    deriving(Show)
type edge = Pos | Neg
    deriving(Show)

type bank_number = Bank of int | TxBank of side * int | Bank_none
    deriving(Show)

type vrefb_group = 
    | Vrefb of int * int
    | Vrefb_none
    deriving(Show)

type pin_fn = 
    | Pfn_none
    | Pfn_io
    | Pfn_nc
    | Pfn_nstatus
    | Pfn_dclk
    | Pfn_nconfig
    | Pfn_tdi
    | Pfn_tck
    | Pfn_tms
    | Pfn_tdo
    | Pfn_nce
    | Pfn_conf_done
    | Pfn_gnd
    | Pfn_vccint
    | Pfn_clk of int
    | Pfn_msel of int
    | Pfn_gnda of int
    | Pfn_vccio of int
    | Pfn_vcca of int
    | Pfn_vcc_pll of char * int
    | Pfn_vcc_gxb of char 
    | Pfn_vcc_clkin of int * char 
    | Pfn_gxb_tx of int * char
    | Pfn_gxb_rx of int * char
    | Pfn_rref of int
    | Pfn_refclk of int * char
    | Pfn_clkio of int 
    deriving(Show)

type opt_fn = 
    | Ofn_diffio of int * side * edge
    | Ofn_vrefb of int * int
    | Ofn_diffclk of int * edge
    | Ofn_rup of int
    | Ofn_rdn of int
    | Ofn_pll_clkout of int * edge
    | Ofn_refclk of int * char
    | Ofn_clkio of int 
    deriving(Show)

type cfg_fn = 
    | Cfn_nreset
    | Cfn_data of int
    | Cfn_asdo
    | Cfn_flash_nce
    | Cfn_ncso
    | Cfn_nstatus
    | Cfn_dclk
    | Cfn_nconfig
    | Cfn_tdi
    | Cfn_tck
    | Cfn_tms
    | Cfn_tdo
    | Cfn_nce
    | Cfn_dev_oe
    | Cfn_dev_clrn
    | Cfn_conf_done
    | Cfn_msel of int
    | Cfn_init_done
    | Cfn_crc_error
    | Cfn_nceo
    | Cfn_clkusr
    | Cfn_nwe
    | Cfn_noe
    | Cfn_navd
    | Cfn_padd of int
    | Cfn_none
    deriving(Show)

type dqs = 
    | DQ of int * side
    | DM of int * side
    | DQS of int * side
    | CQ of int * side
    | CDPCLK of int
    | BWS of int * side
    | DPCLK of int
    deriving(Show)

(***************************************************************************)
(* token parsers *)

exception Parse_failure of string * string
let parse_fail m e = raise (Parse_failure(m, e))

RE intx = int as x : int
RE inty = int as y : int
RE charc = alnum as c := (fun s -> s.[0])
RE chard = alnum as d := (fun s -> s.[0])
RE side = ["L" "R" "B" "T"] as side := 
    (function "L" -> Left
            | "R" -> Right
            | "T" -> Top
            | "B" -> Bottom
            | _   -> failwith "invalid side")
RE edge = ["p" "n"] as edge := 
    (function "p" -> Pos
            | "n" -> Neg
            | _   -> failwith "invalid edge")

let bank_number_of_string s = 
    match s with
    | RE eol -> Bank_none
    | RE "B" intx -> Bank x
    | RE "Q" side intx -> TxBank(side, x)
    | _ -> parse_fail "bank_number" s

let vrefb_of_string s = 
    match s with
    | RE eol -> Vrefb_none
    | RE "VREFB" intx "N" inty -> Vrefb(x,y)
    | _ -> parse_fail "vrefb" s

let pin_fn_of_string s = 
    match s with
    | RE eol -> Pfn_none
    | RE "IO" -> Pfn_io
    | RE "NC" -> Pfn_nc
    | RE "nSTATUS" -> Pfn_nstatus
    | RE "DCLK" -> Pfn_dclk
    | RE "nCONFIG" -> Pfn_nconfig
    | RE "TDI" -> Pfn_tdi
    | RE "TCK" -> Pfn_tck
    | RE "TMS" -> Pfn_tms
    | RE "TDO" -> Pfn_tdo
    | RE "nCE" -> Pfn_nce
    | RE "GND" -> Pfn_gnd
    | RE "VCCINT" -> Pfn_vccint
    | RE "CONF_DONE" -> Pfn_conf_done
    | RE "CLK" intx -> Pfn_clk(x)
    | RE "MSEL" intx -> Pfn_msel(x)
    | RE "GNDA" intx -> Pfn_gnda(x)
    | RE "VCCIO" intx -> Pfn_vccio(x)
    | RE "VCCA" intx -> Pfn_vcca(x)
    | RE "VCCA" -> Pfn_vcca(0)
    | RE "VCC" charc "_PLL" intx -> Pfn_vcc_pll(c, x)
    | RE "VCC" charc "_PLL" -> Pfn_vcc_pll(c, 0)
    | RE "VCC" charc "_GXB" -> Pfn_vcc_gxb(c)
    | RE "VCC_CLKIN" intx charc -> Pfn_vcc_clkin(x, c)
    | RE "GXB_TX" intx charc -> Pfn_gxb_tx(x, c)
    | RE "GXB_RX" intx charc -> Pfn_gxb_rx(x, c)
    | RE "RREF" intx -> Pfn_rref(x)
    | RE "REFCLK" intx charc -> Pfn_refclk(x, c)
    | RE "CLKIO" intx -> Pfn_clkio(x)
    | _ -> parse_fail "pin_fn" s

let opt_fn_of_string s = 
    match s with
    | RE "DIFFIO_" side intx edge -> Ofn_diffio(x, side, edge)
    | RE "VREFB" intx "N" inty -> Ofn_vrefb(x, y)
    | RE "DIFFCLK_" intx edge -> Ofn_diffclk(x, edge)
    | RE "RUP" intx -> Ofn_rup(x)
    | RE "RDN" intx -> Ofn_rdn(x)
    | RE "PLL" intx "_CLKOUT" edge -> Ofn_pll_clkout(x, edge)
    | RE "REFCLK" intx charc -> Ofn_refclk(x, c)
    | RE "CLKIO" intx -> Ofn_clkio(x)
    | _ -> parse_fail "opt_fn" s

let cfg_fn_of_string s = 
    match s with
    | RE eol -> Cfn_none
    | RE "nRESET" -> Cfn_nreset
    | RE "DATA" intx -> Cfn_data(x)
    | RE "ASDO" -> Cfn_asdo
    | RE "FLASH_nCE" -> Cfn_flash_nce
    | RE "nCSO" -> Cfn_ncso
    | RE "NCSO" -> Cfn_ncso
    | RE "nSTATUS" -> Cfn_nstatus
    | RE "DCLK" -> Cfn_dclk
    | RE "nCONFIG" -> Cfn_nconfig
    | RE "TDI" -> Cfn_tdi
    | RE "TCK" -> Cfn_tck
    | RE "TMS" -> Cfn_tms
    | RE "TDO" -> Cfn_tdo
    | RE "nCE" -> Cfn_nce
    | RE "DEV_OE" -> Cfn_dev_oe
    | RE "DEV_CLRn" -> Cfn_dev_clrn
    | RE "CONF_DONE" -> Cfn_conf_done
    | RE "MSEL" intx -> Cfn_msel(x)
    | RE "INIT_DONE" -> Cfn_init_done
    | RE "CRC_ERROR" -> Cfn_crc_error
    | RE "nCEO" -> Cfn_nceo
    | RE "NCEO" -> Cfn_nceo
    | RE "CLKUSR" -> Cfn_clkusr
    | RE "nWE" -> Cfn_nwe
    | RE "nOE" -> Cfn_noe
    | RE "nAVD" -> Cfn_navd
    | RE "PADD" intx -> Cfn_padd(x)
    | _ -> parse_fail "opt_fn" s
 
let dqs_of_string s = 
    match s with
    | RE "DQ" intx side -> DQ(x, side)
    | RE "DM" intx side -> DM(x, side)
    | RE "DQS" intx side -> DQS(x, side)
    | RE "CQ" intx side -> CQ(x, side)
    | RE "CDPCLK" intx -> CDPCLK(x)
    | RE "BWS#" intx side -> BWS(x, side)
    | RE "DPCLK" intx -> DPCLK(x)
    | _ -> parse_fail "dqs" s

(***************************************************************************)
(* pin parser *)

type ('a,'b) either = A of 'a | B of 'b
    deriving(Show)

type pin_attrib = 
    | BankNo of bank_number
    | VrefbGroup of vrefb_group
    | PinFn of pin_fn
    | OptFn of opt_fn list
    | CfgFn of cfg_fn list
    | Pin of string
    | DqsFn of (string, dqs list) either
    deriving(Show)

let show_pin_attrib = function
    | BankNo(x) -> "BankNo(" ^ Show.show<bank_number> x ^ ")"
    | VrefbGroup(x) -> "VrefbGroup(" ^ Show.show<vrefb_group> x ^ ")"
    | PinFn(x) -> "PinFn(" ^ Show.show<pin_fn> x ^ ")"
    | OptFn(x) -> "OptFn(" ^ Show.show_list<opt_fn> x ^ ")"
    | CfgFn(x) -> "CfgFn(" ^ Show.show_list<cfg_fn> x ^ ")"
    | Pin(x) -> "Pin(" ^ Show.show<string> x ^ ")"
    | DqsFn(x) -> "DqsFn(" ^ Show.show<(string,dqs list) either> x ^ ")"

type pin_attribs = pin_attrib array

type pins_attribs = pin_attribs array

type pin_parse_spec = int * int * pin_attribs

let prep_token tok = 
    match tok with
    | RE "\"" (_* Lazy as tok) "\"" -> tok
    | _ -> tok
let prep_token tok = Mikmatch.Fixed.chop_spaces (prep_token tok)

(* remove end of line chars (newline etc), split by tabs *)
let prep_tokens line = 
    let line = (* hack - leading tab is ignored by SPLIT. *) 
        if line.[0] = '\t' then "\t" ^ line
        else line
    in
    match line with 
    | RE (_* Lazy as strs := (SPLIT "\t")) ["\r" "\n"]* eol -> 
        Array.of_list (List.map prep_token strs)
    | _ -> [||]

let parse_pin spec line = 
    let tokens = prep_tokens line in
    let token i = try tokens.(i) with _ -> "" in
    let token_list i =
        List.map Mikmatch.Fixed.chop_spaces ((SPLIT ["," "/"]) (token i))
    in
    Array.mapi (fun i spec ->
        match spec with
        | BankNo(_) -> BankNo(bank_number_of_string (token i))
        | VrefbGroup(_) -> VrefbGroup(vrefb_of_string (token i))
        | PinFn(_) -> PinFn(pin_fn_of_string (token i))
        | OptFn(_) -> OptFn(List.map opt_fn_of_string (token_list i))
        | CfgFn(_) -> CfgFn(List.map cfg_fn_of_string (token_list i))
        | Pin(_) -> Pin(token i)
        | DqsFn(A(_)) -> DqsFn(B(List.map dqs_of_string (token_list i)))
        | _ -> failwith "bad pin attribute spec"
    ) spec

let parse_pins (skip,cnt,spec) f = 
    for i=0 to skip-1 do
        ignore (input_line f)
    done;
    Array.init cnt (fun _ -> parse_pin spec (input_line f))

(***************************************************************************)
(* a few CycloneIV pin specs *)

let ep4ce22 = 
    5, 257, [|
        BankNo(Bank_none);
        VrefbGroup(Vrefb_none);
        PinFn(Pfn_nc);
        OptFn([]);
        CfgFn([]);
        Pin("F/U256");
        Pin("E144");
        DqsFn(A("X8/9 F/U256"));
        DqsFn(A("X16/18 F/U256"));
        DqsFn(A("X8/9 E144"));
    |]
        

let ep4cgx15 = 
    6, 173, [|
        BankNo(Bank_none);
        VrefbGroup(Vrefb_none);
        PinFn(Pfn_nc);
        OptFn([]);
        CfgFn([]);
        Pin("F169");
        Pin("Q148");
        DqsFn(A("X8 F169"));
        DqsFn(A("X8 Q148"));
    |]

let ep4cgx150 = 
    6, 896, [|
        BankNo(Bank_none);
        VrefbGroup(Vrefb_none);
        PinFn(Pfn_nc);
        OptFn([]);
        CfgFn([]);
        Pin("F896");
        Pin("F672");
        Pin("F484");
        DqsFn(A("X8/9 F896"));
        DqsFn(A("X16/18 F896"));
        DqsFn(A("X32/36 F896"));
        DqsFn(A("X8/9 F672"));
        DqsFn(A("X16/18 F672"));
        DqsFn(A("X32/36 F672"));
        DqsFn(A("X8/9 F484"));
        DqsFn(A("X16/18 F484"));
        DqsFn(A("X32/36 F484"));
    |]

let gen_fpgas ?(path="board/data/") file_out = 
    let fpgas = 
        [
            "ep4ce22", ep4ce22, "EP4CE22.txt";
            "ep4cgx15", ep4cgx15, "EP4CGX15.txt";
            "ep4cgx150", ep4cgx150, "EP4CGX150.txt";
        ]
    in
    lwt () = Lwt_io.write_line file_out "open Altera_pins" in
    Lwt_list.iter_s
        (fun (fpga,(skip,cnt,spec),file_in) ->
            lwt file_in = Lwt_io.(open_file ~mode:input (path ^ file_in)) in
            (* skip initial lines *)
            lwt () =
               for_lwt i=0 to skip-1 do
                   lwt _ = Lwt_io.read_line file_in in Lwt.return ()
               done
            in
            let write pin = 
                lwt () = Lwt_io.write_line file_out "[|" in
                lwt () = Lwt_list.iter_s
                    (fun p ->
                        let s = show_pin_attrib p in
                        Lwt_io.write_line file_out (s ^ ";")
                    ) (Array.to_list pin)
                in
                lwt () = Lwt_io.write_line file_out "|];" in
                Lwt.return ()
            in
            (* parse and write 'cnt' lines *)
            let rec parse n = 
                if n = cnt then Lwt.return ()
                else
                    lwt line = Lwt_io.read_line file_in in
                    let pin = parse_pin spec line in
                    lwt () = write pin in
                    parse (n+1)
            in
            lwt () = Lwt_io.write_line file_out ("let " ^ fpga ^ " = [") in
            lwt () = parse 0 in
            lwt () = Lwt_io.write_line file_out "]" in

            lwt () = Lwt_io.close file_in in
            Lwt.return ()
        ) fpgas


(*
# run $ Lwt_io.(with_file ~mode:output "altera_fpgas.ml" UJamJarX.Altera_pins.gen_fpgas;;
*)

