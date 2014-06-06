(*val tokens : string -> string array
val token_list : string -> string list*)

type side = Left | Bottom | Right | Top

type edge = Pos | Neg

type bank_number = Bank of int | TxBank of side * int | Bank_none

type vrefb_group = 
    | Vrefb of int * int
    | Vrefb_none

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

type opt_fn = 
    | Ofn_diffio of int * side * edge
    | Ofn_vrefb of int * int
    | Ofn_diffclk of int * edge
    | Ofn_rup of int
    | Ofn_rdn of int
    | Ofn_pll_clkout of int * edge
    | Ofn_refclk of int * char
    | Ofn_clkio of int 

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

type dqs = 
    | DQ of int * side
    | DM of int * side
    | DQS of int * side
    | CQ of int * side
    | CDPCLK of int
    | BWS of int * side
    | DPCLK of int

type ('a,'b) either = A of 'a | B of 'b

type pin_attrib = 
    | BankNo of bank_number
    | VrefbGroup of vrefb_group
    | PinFn of pin_fn
    | OptFn of opt_fn list
    | CfgFn of cfg_fn list
    | Pin of string
    | DqsFn of (string,dqs list) either

type pin_attribs = pin_attrib array
type pins_attribs = pin_attribs array
type pin_parse_spec = int * int * pin_attribs 

val prep_tokens : string -> string array 


val parse_pin : pin_attribs -> string -> pin_attribs 
val parse_pins : pin_parse_spec -> in_channel -> pins_attribs 

val ep4ce22 : pin_parse_spec
val ep4cgx15 : pin_parse_spec
val ep4cgx150 : pin_parse_spec

val gen_fpgas : ?path:string -> Lwt_io.output Lwt_io.channel -> unit Lwt.t

