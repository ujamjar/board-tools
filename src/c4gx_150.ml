let board_settings () = 
    let open Altera_qsf in
    let path = Sys.getenv "UJAMJAR_DATA" in
    let qsf_file = Filename.concat path "c4gx_150.qsf" in
    lwt c4gx_150 = parse_stream qsf_file in
    Lwt.return {
        part = "EP4CGX15BF14C6";
        top_level_entity = "c4gx_150";
        files = [];
        default_io_std = 
            (try get_global "STRATIX_DEVICE_IO_STANDARD" c4gx_150
            with Not_found -> "2.5 V");
        reserve_after_config = get_reserve_after_config c4gx_150;
        pins = get_pin_info c4gx_150;
    }
