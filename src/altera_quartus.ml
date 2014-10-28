open Lwt
open Utils

(* quartus device package *)
module Device = struct
    
    open Options

    let get_family_list p =
        lwt s = Utils.(command string) "get_family_list\n" p in
        Utils.split_tokens s

    type part_info_v = [
        | `Device 
        | `Family 
        | `Package 
        | `Pins 
        | `Speed 
    ]

    type get_part_info_opts_v = [
        | part_info_v
        | `Default_voltage 
        | `Family_variant 
        | `Temp_grade 
    ]

    let get_part_info ~opts part p = 
        let opt = function
            | `Default_voltage -> "-default_voltage"
            | `Device -> "-device"
            | `Family -> "-family"
            | `Family_variant -> "-family_variant"
            | `Package -> "-package"
            | `Pins -> "-pin_count"
            | `Speed -> "-speed"
            | `Temp_grade -> "-temperature_grade"
        in
        match_lwt Utils.(command (string-:option))  
            (buildl "get_part_info" ~post:part ~args:(fold opt opts)) p with
        | None -> return ""
        | Some(x) -> return x

    let get_part_list ?(opts=[]) p = 
        let opt = function 
            | `Device,(_ as s) -> "-device " ^ s
            | `Family,(_ as s) -> "-family " ^ s
            | `Package,(_ as s) -> "-package " ^ s
            | `Pins,(_ as s) -> "-pin_count " ^ s
            | `Speed,(_ as s) -> "-speed " ^ s 
        in
        match_lwt Utils.(command (string-:option)) 
                    (buildl "get_part_list" ~args:(fold opt opts)) p with
        | None -> return []
        | Some(s) -> return ((SPLIT " ") s)

    let report_device_info dev = Utils.(command strings) 
        (buildl "report_device_info" ~post:dev)

    let report_family_info family = Utils.(command strings) 
        (buildl "report_family_info " ~post:family)

    type part_info =
        {
            part : string;
            family : string;
            device : string;
            package : string;
            pin_count : int;
            speed_grade : string;
            io_count : int;
            lcell_count : int;
        }
    
    type part_info_ex = 
        {
            part_info : part_info;
            default_voltage : string;
            family_variant : string;
            temp_grade : string;
        }

    RE intx = int as x : int
    RE str = _+ Lazy as s

    let report_part_info part = 
        let conv line = 
            match line with
            | RE "Part Name  : " str eol -> `Part, `Str s
            | RE "Family Name  : " str eol -> `Family, `Str s
            | RE "Device Name  : " str eol -> `Device, `Str s
            | RE "Package Name : " str eol -> `Package, `Str s
            | RE "Pin Count    : " intx eol -> `Pins, `Int x
            | RE "Speed Grade  : " str eol -> `Speed,`Str s
            | RE "I/O Count    : " intx eol ->`Ios, `Int x
            | RE "Lcell Count  : " intx eol -> `Lcells, `Int x
            | _ -> `None, `Str line
        in
        let fpga_part l = 
            let ustr = function `Str s -> s | _ -> failwith "ustr" in
            let uint = function `Int x -> x | _ -> failwith "uint" in
            let assoc = List.assoc in
            {   
                part = ustr (assoc `Part l);
                family = ustr (assoc `Family l);
                device = ustr (assoc `Device l);
                package = ustr (assoc `Package l);
                pin_count = uint (assoc `Pins l);
                speed_grade = ustr (assoc `Speed l);
                io_count = uint (assoc `Ios l);
                lcell_count = uint (assoc `Lcells l);
            }
        in
        Utils.(command
            (fun stream -> 
                let stream = Lwt_stream.map conv stream in
                lwt list = Lwt_stream.to_list stream in
                (wrap1 fpga_part) list)
            (buildl "report_part_info " ~post:part)) 

    let report_part_info_ex part p = 
        lwt part_info = report_part_info part p in
        lwt default_voltage = get_part_info ~opts:[`Default_voltage] part p in
        lwt family_variant = get_part_info ~opts:[`Family_variant] part p in
        lwt temp_grade = get_part_info ~opts:[`Temp_grade] part p in
        return 
            { 
                part_info;
                default_voltage;
                family_variant;
                temp_grade;
            }

    let rec map_s f l = 
        match l with
        | [] -> return []
        | h::t ->
            lwt x = f h in
            lwt y = map_s f t in
            return (x :: y)

    let get_all_parts p = 
        lwt parts = get_part_list p in
        lwt parts = map_s (fun s -> report_part_info s p) parts in
        return parts

    let get_all_parts_ex p = 
        lwt parts = get_part_list p in
        Lwt_list.map_s (fun s -> report_part_info_ex s p) parts 

end

module Misc = struct 
    
    open Options

    let checksum ?(algorithm=`Crc32) ~filename = 
        Utils.command_string
            (buildl ("checksum " ^ filename)
                ~args:(arg1_of "-algorithm"
                    (function `Crc32 -> "crc32"
                            | `Adler32 -> "adler32") algorithm)) 

    let disable_natural_bus_mapping = Utils.command_unit 
        (buildl "disable_natural_bus_mapping")
    let enable_natural_bus_mapping = Utils.command_unit 
        (buildl "enable_natural_bus_mapping")
    
    let escape_brackets str = Utils.command_string (buildl ("escape_brackets " ^ str))

    let foreach_in_collection ~var ~collection ~body = 
        Utils.command_strings
            (buildl ("foreach_in_collection " ^ var ^ " " ^ collection ^ " " ^ body))
    let get_collection_size ~collection =
        Utils.command_int (buildl ("get_collection_size " ^ collection))

    let get_environment_info opt = 
        Utils.command_string
            (buildl "get_environment_info"
                ~args:(flag1_of (function
                    | `Num_logical_processors -> "-num_logical_processors"
                    | `Num_physical_processors -> "-num_physical_processors"
                    | `Operating_system -> "-operating_system")
                opt))
            
    let init_tk = Utils.command_unit "init_tk\n"

    let load args = 
        Utils.command_unit
            (buildl "load" ~args:(strings args))
                
    let load_package ?version package = 
        Utils.command_unit
            (buildl ("load_package " ^ package)
                ~args:(string_opt "-version" version))

    let post_message ?file ?line ?(typ=`Info) message = 
        let typ' = function 
            | `Info -> "-type info"
            | `Warning -> "-type warning"
            | `Error -> "-type error"
            | `Extra_info -> "-type extra_info"
            | `Critical_warning -> "-type critical_warning"
            | _ -> ""
        in
        Utils.command_unit
            (buildl "post_message" 
                ~args:(arg_string_opt "-file" file ^
                       arg_int_opt "-line" line ^
                       flag1_of typ' typ)
                ~post:message)

    let qexec command = Utils.command_strings (buildl ("qexec " ^ command))

    let qexit status =
        Utils.command_unit 
            (buildl "qexit"
                ~args:(flag1_of
                    (function `Error -> "-error"
                            | `Success -> "-success") status))

    let stopwatch arg = 
        Utils.command_string_opt
            (buildl "stopwatch"
                ~args:(flag1_of
                    (function
                        | `Lap_time -> "-lap_time"
                        | `Number_format -> "-number_format"
                        | `Reset -> "-reset"
                        | `Start -> "-start")
                    arg))

end

module Flow = struct

    open Options

    let execute_flow
        ?dont_export_assignments 
        flow
        =
        let arg = function
            | `Analysis_and_elaboration -> "-analysis_and_elaboration"
            | `Check_ios -> "-check_ios"
            | `Check_netlist -> "-check_netlist" 
            | `Compile -> "-compile"
            | `Compile_and_simulate -> "-compile_and_simulate" 
            | `Create_companion_revision -> "-create_companion_revision"
            | `Early_timing_estimate -> "-early_timing_estimate" 
            | `Eco -> "-eco" 
            | `Eda_synthesis -> "-eda_synthesis" 
            | `Export_database -> "-export_database"
            | `Fast_model -> "-fast_model"
            | `Generate_functional_sim_netlist -> "-generate_functional_sim_netlist" 
            | `Import_database -> "-import_database"
            | `Incremental_compilation_export -> "-incremental_compilation_export" 
            | `Incremental_compilation_import -> "-incremental_compilation_import"
            | `Signalprobe -> "-signalprobe"
            | `Vqm_writer -> "-vqm_writer"
        in
        let args = " " ^ arg flow in
        let args = args ^ flag_opt "-dont_export_assignments" dont_export_assignments in
        Utils.command_catch (build "execute_flow" ~args)

    let execute_hc ?archive ?compare ?create_companion
        ?handoff_report ?hc_ready ?min_archive = 
        let args = arg_string_opt "-archive" archive in
        let args = args ^ flag_opt "-compare" compare in
        let args = args ^ arg_string_opt "-create_companion" create_companion in
        let args = args ^ flag_opt "-handoff_report" handoff_report in
        let args = args ^ flag_opt "-hc_ready" hc_ready in
        let args = args ^ flag_opt "-min_archive" min_archive in
        Utils.command_catch (build "execute_hc" ~args)

    let execute_module ?args ?dont_export_assignments ~tool = 
        let args = arg_string_opt "-args" args in
        let args = args ^ flag_opt "-dont_export_assignments" dont_export_assignments in
        let args = args ^ arg1_of "-tool" 
            (function
                |`Asm->"asm"|`Cdb->"cdb"|`Drc->"drc"
                |`Eda->"eda"|`Fit->"fit"|`Map->"map"
                |`Pow->"pow"|`Sta->"sta"|`Sim->"sim"
                |`Tan->"tan"|`Si->"si"|`Cpf->"cpf")
            tool
        in
        Utils.command_catch (build "exeecute_module" ~args)

end

module Report = struct
    
    open Options

    type panel_id = [ `Id of int | `Name of string ]

    let id_arg ?(i="id") ?(n="name") id = 
        " -" ^ (match id with
        | `Id x -> i ^ " " ^ string_of_int x
        | `Name x -> n ^ " "  ^ x)

    let add_row_to_table ~id row = 
        Utils.command_unit (buildl "add_row_to_table" ~args:(id_arg id) ~post:row)

    let create_report_panel ?folder ?table panel_name = 
        let args = flag_opt "-folder" folder ^ flag_opt "-table" table in
        Utils.command_unit (buildl "create_report_panel" ~args:args ~post:panel_name)

    let delete_report_panel ~id = 
        Utils.command_unit (buildl "delete_report_panel" ~args:(id_arg id))

    type f_resource = [`Alm|`Alut|`Io_pin|`Lab|`Le|`Mem_bit|`Reg] 
    type f_resource_use = [`Available|`Used|`Percentage]
    type f_resource_full = [f_resource|f_resource_use|`Utilization]

    let fitter_flag = function
             `Alm->"-alm"|`Alut->"-alut"|`Available->"-available"
            |`Io_pin->"-io_pin"|`Lab->"-lab"|`Le->"-le"
            |`Mem_bit->"-mem_bit"|`Percentage->"-percentage"
            |`Reg->"-reg"|`Used->"-used"|`Utilization->"-utilization"

    let get_fitter_resource_usage flags = 
        let args = fold fitter_flag flags in
        Utils.command_strings (buildl "get_fitter_resource_usage " ~args:args)

    let get_fitter_resource typ flag = 
        let args = " " ^ fitter_flag typ in
        let args = args ^ " " ^ fitter_flag flag in
        Utils.command_int (buildl "get_fitter_resource_usage" ~args:args)

    type resources = 
        {
            alm : int * int;
            alut : int * int;
            io_pin : int * int;
            lab : int * int;
            le : int * int;
            mem_bit : int * int;
            reg : int * int;
        }

    let get_fitter_report p = 
        let get typ = 
            lwt used = get_fitter_resource typ `Used p in
            lwt avail = get_fitter_resource typ `Available p in
            return (used, avail)
        in
        lwt alm = get `Alm in
        lwt alut = get `Alut in
        lwt io_pin = get `Io_pin in
        lwt lab = get `Lab in
        lwt le = get `Le in
        lwt mem_bit = get `Mem_bit in
        lwt reg = get `Reg in
        return { alm; alut; io_pin; lab; le; mem_bit; reg }

    let get_number_of_columns ~id = 
        Utils.command_int (buildl "get_number_of_columns" ~args:(id_arg id))

    let get_number_of_rows ~id = 
        Utils.command_int (buildl "get_number_of_rows" ~args:(id_arg id))

    let get_report_panel_column_index ~id col_name = 
        Utils.command_int (buildl "get_report_panel_column_index" 
            ~args:(id_arg id) ~post:col_name)

    let get_report_panel_row_index ~id row_name = 
        Utils.command_int (buildl "get_report_panel_row_index" 
            ~args:(id_arg id) ~post:row_name)

    let get_report_panel_data ~id ~col ~row = 
        let args = id_arg id ^ id_arg ~i:"col" ~n:"col_name" col ^ 
                               id_arg ~i:"row" ~n:"row_name" row
        in
        Utils.command_strings (buildl "get_report_panel_data" ~args:args) 

    let get_report_panel_id name = 
        Utils.command_int (buildl "get_report_panel_id" ~post:name)

    let get_report_panel_names = Utils.command_string (buildl "get_report_panel_names")

    let get_report_panel_row ~id ~row = 
        let args = id_arg id ^ id_arg ~i:"row" ~n:"row_name" row in
        Utils.command_strings (buildl "get_report_panel_row" ~args:args) 

    let get_timing_analysis_summary_results what how =
        let args = " -" ^ ((function
            | `Tsu -> "tsu"
            | `Tco -> "tco"
            | `Tpd -> "tpd"
            | `Th -> "th"
            | `Min_tco -> "min_tco"
            | `Min_tpd -> "min_tpd"
            | `Clock_setup c -> "clock_setup " ^ c
            | `Clock_hold h -> "clock_hold " ^ h) what)
        in
        let args = args ^ " -" ^ ((function
            | `Slack -> "slack"
            | `Required -> "required"
            | `Actual -> "actual") how)
        in
        Utils.command_string (buildl "get_timing_analysis_summary_results" ~args:args)

    let load_report ?simulator = 
        Utils.command_unit (buildl "load_report" ~args:(flag_opt "-simulator" simulator))

    let read_xml_report name = 
        Utils.command_unit (buildl "read_xml_report" ~post:name)

    let save_report_database = Utils.command_unit (buildl "save_report_database")
    
    let unload_report = Utils.command_unit (buildl "unload_report")

    let write_report_panel ~id ?html ?xml ~file = 
        let args = " -file " ^ file in
        let args = args ^ id_arg id ^ flag_opt "-html" html ^ 
                                      flag_opt "-xml" xml 
        in
        Utils.command_unit (buildl "write_report_panel" ~args:args)

    let write_xml_report file = 
        Utils.command_unit (buildl "write_xml_report" ~args:file)

end

module Project = struct

    open Options

    let assignment_group 
        ?add_exception ?add_member
        ?comment ?disable ?get_exceptions ?get_members 
        ?overwrite ?remove ?remove_exception 
        ?remove_member ?tag
        group_name =

        let args = 
            arg_string_opt "-add_exception" add_exception ^
            arg_string_opt "-add_member" add_member ^
            arg_string_opt "-comment" comment ^
            flag_opt "-disable" disable ^
            flag_opt "-get_exceptions" get_exceptions ^
            flag_opt "-get_members" get_members ^
            flag_opt "-overwrite" overwrite ^
            flag_opt "-remove" remove ^ 
            arg_string_opt "-remove_exception" remove_exception ^
            arg_string_opt "-remove_member" remove_member ^
            arg_string_opt "-tag" tag 
        in
        Utils.command_unit (buildl "assignment_group" ~args:args ~post:group_name)

    let create_revision 
        ?based_on ?copy_results ?set_current 
        rev_name =
        let args = 
            arg_string_opt "-based_on" based_on ^
            flag_opt "-copy_results" copy_results ^
            flag_opt "-set_current" set_current
        in
        Utils.command_unit (buildl "create_revision" ~args:args ~post:rev_name)

    let delete_revision rev_name = 
        Utils.command_unit (buildl "delete_revision" ~post:rev_name)

    let export_assignments ?reorganise = 
        Utils.command_unit (buildl "export_assignments" 
            ~args:(flag_opt "-reorganise" reorganise))

    let get_all_assignment_names ?family ?(modl=`All) ?(typ=`All) = 
        let args = 
            arg_string_opt "-family" family ^
            flag1_of (function
                | `All -> "-all"
                | `Map -> "-map"
                | `Fit -> "-fit"
                | `Tan -> "-tan"
                | `Asm -> "-asm"
                | `Eda -> "-eda"
                | `Drc -> "-drc"
                | `Generic -> "-generic") modl ^ 
            flag1_of (function
                | `All -> "-all"
                | `Global -> "-global"
                | `Instance -> "-instance") typ
        in
        Utils.command_string (buildl "get_all_assignment_names" ~args:args)

    let get_all_assignments  ?entity ?from ?section_id ?tag ?to_ ~name ~typ =
        let args = 
            arg_string_opt "-entity" entity ^ 
            arg_string_opt "-from" from ^
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name ^ 
            (arg1_of "-type" (function
                | `Global -> "global"
                | `Instance -> "instance"
                | `Parameter -> "parameter"
                | `Default -> "default") typ)
        in
        Utils.command_string (buildl "get_all_assignments" ~args)

    let get_all_global_assignments ?entity ?section_id ?tag ~name =
        let args = 
            arg_string_opt "-entity" entity ^ 
            arg_int_opt "-section_id" section_id ^
            " -name " ^ name 
        in
        Utils.command_string (buildl "get_all_global_assignments" ~args)

    let get_all_instance_assignments ?entity ?from ?section_id ?tag ?to_ ~name =
        let args = 
            arg_string_opt "-entity" entity ^ 
            arg_string_opt "-from" from ^
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name  
        in
        Utils.command_string (buildl "get_all_instance_assignments" ~args)

    let get_all_parameters ?entity ?tag ?to_ ~name =
        let args = 
            arg_string_opt "-entity" entity ^ 
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name  
        in
        Utils.command_string (buildl "get_all_parameters" ~args)

    let get_all_quartus_defaults ?section_id ?name = 
        let args = 
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-name" name 
        in
        Utils.command_string (buildl "get_all_quartus_defaults" ~args)

    let get_all_user_option_names ?name = 
        let args = arg_string_opt "-name" name in
        Utils.command_string (buildl "get_all_user_option_names" ~args)

    let get_assignment_info ~id typ = 
        let args = flag1_of (function
            | `Entity -> "-entity"
            | `From -> "-from"
            | `Get_tcl_command -> "-get_tcl_command"
            | `Name -> "-name"
            | `Section_id -> "-section_id"
            | `Tag -> "-tag"
            | `To -> "-to"
            | `Value -> "-value") typ
        in
        Utils.command_string 
            (buildl "get_assignment_info" ~args ~post:(string_of_int id))

    let get_current_revision name = 
        Utils.command_string (buildl "get_current_revision" ~post:name)

    let get_global_assignment ?entity ?front ?section_id ?tag ~name =
        let args = 
            arg_string_opt "-entity" entity ^
            flag_opt "-front" front ^  
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-tag" tag ^
            " -name " ^ name  
        in
        Utils.command_string (buildl "get_global_assignment" ~args)

    let get_instance_assignment ?entity ?from ?front ?section_id ?tag ?to_ ~name =
        let args = 
            arg_string_opt "-entity" entity ^
            arg_string_opt "-from" from ^
            flag_opt "-front" front ^  
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name  
        in
        Utils.command_string (buildl "get_instance_assignment" ~args)

    let get_location_assignment ?tag ~to_ = 
        let args = 
            arg_string_opt "-tag" tag ^ 
            " -to " ^ to_
        in
        Utils.command_string (buildl "get_location_assignment" ~args)

    type observable_type_v = 
        [`All|`Pre_synthesis|`Post_synthesis|`Post_fitter
        |`Post_asm|`Stp_pre_synthesis] 

    let arg_observable_type = 
        arg1_of "-observable_type" (function
             `All -> "all"
            |`Pre_synthesis -> "pre_synthesis"
            |`Post_synthesis -> "post_synthesis"
            |`Post_fitter -> "post_fitter"
            |`Post_asm -> "post_asm"
            |`Stp_pre_synthesis -> "stp_pre_synthesis")

    let get_name_info ?get_synonyms ~info
        ?(observable_type=`All) ?use_cached_database
        id = 
        let args = 
            flag_opt "-get_synonyms" get_synonyms ^ 
            arg1_of "-info" (function
                 `Parent_name_id -> "parent_name_id"
                |`Base_name -> "base_name"
                |`Entity_name -> "entity_name"
                |`Instance_name -> "instance_name"
                |`Full_path -> "full_path"
                |`Short_full_path -> "short_full_path"
                |`Node_type -> "node_type"
                |`Creator -> "creator"
                |`Signaltapii -> "signaltapii"
                |`File_location -> "file_location"
                |`Library -> "library"
                |`Children -> "children"
                |`Parameters -> "parameters") info ^ 
            arg_observable_type observable_type ^
            flag_opt "-use_cached_database" use_cached_database  
             
        in
        Utils.command_string (buildl "get_name_info" ~args)

    let get_names ?entity ~filter ?library ~node_type
        ?(observable_type=`All) = 
        let args = 
            arg_string_opt "-entity" entity ^ 
            " -filter " ^ filter ^
            arg_string_opt "-library" library ^ 
            arg1_of "-node_type" (function
                | `All -> "all"
                | `Comb -> "comb"
                | `Reg -> "reg" 
                | `Pin -> "pin"
                | `Input -> "input"
                | `Output -> "output"
                | `Bidir -> "bidir"
                | `Hierarchy -> "hierarchy"
                | `Mem -> "mem"
                | `Bus -> "bus"
                | `Qsf -> "qsf") node_type ^
            arg_observable_type observable_type 
        in
        Utils.command_string (buildl "get_names" ~args)

    let get_parameter ?entity ~name ?tag ?to_ = 
        let args = 
            arg_string_opt "-entity" entity ^ 
            " -name" ^ name ^
            arg_string_opt "-tag" tag ^ 
            arg_string_opt "-to" to_
        in
        Utils.command_string (buildl "get_parameter" ~args) 

    let get_project_directory = Utils.command_string (buildl "get_project_directory")

    let get_project_revisions name = 
        Utils.command_string (buildl "get_project_revisions" ~post:name)

    let get_top_level_entity = Utils.command_string (buildl "get_top_level_entity")

    let get_user_option name = 
        Utils.command_string (buildl "get_user_option" ~post:name)

    let is_project_open = Utils.command_bool (buildl "is_project_open")

    let project_archive  
        ?all_revisions ?include_libraries 
        ?include_outputs ?overwrite 
        ?use_file_set ?version_compatible_database 
        name = 
        let args = 
            flag_opt "-all_revisions" all_revisions ^
            flag_opt "-include_libraries" include_libraries ^
            flag_opt "-include_outputs" include_outputs ^
            flag_opt "-overwrite" overwrite ^
            flag_opt "-version_compatible_database" version_compatible_database ^
            arg_string_opt "-use_file_set" use_file_set
        in
        Utils.command_unit (buildl "project_archive" ~args)

    let project_close ?dont_export_assignments = 
        let args = flag_opt "-dont_export_assignments" dont_export_assignments in
        Utils.command_unit (buildl "project_close" ~args)

    let project_exists name = 
        Utils.command_bool (buildl "project_exists" ~post:name)

    let project_new ?family ?overwrite ?part ?revision name =
        let args = 
            arg_string_opt "-family" family ^
            flag_opt "-overwrite" overwrite ^
            arg_string_opt "-part" part ^
            arg_string_opt "-revision" revision 
        in 
        Utils.command_catch (build "project_new" ~args ~post:name)
            
    let project_open ?current_revision?force ?revision name =
        let args = 
            flag_opt "-current_revision" current_revision ^
            flag_opt "-force" force ^
            arg_string_opt "-revision" revision 
        in
        Utils.command_catch (build "project_open" ~args ~post:name)

    let project_restore ?destination ?overwrite
        ?update_included_file_info name = 
        let args = 
            arg_string_opt "-destination" destination ^
            flag_opt "-overwrite" overwrite ^
            flag_opt "-update_included_file_info" update_included_file_info 
        in
        Utils.command_unit (buildl "project_restore" ~args ~post:name)

    let remove_all_global_assignments ?entity ?section_id ?tag ~name =
        let args = 
            arg_string_opt "-entity" entity ^ 
            arg_int_opt "-section_id" section_id ^
            " -name " ^ name 
        in
        Utils.command_unit (buildl "remove_all_global_assignments" ~args)

    let remove_all_instance_assignments ?entity ?from ?section_id ?tag ?to_ ~name =
        let args = 
            arg_string_opt "-entity" entity ^ 
            arg_string_opt "-from" from ^
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name  
        in
        Utils.command_string (buildl "remove_all_instance_assignments" ~args)

    let remove_all_parameters ?entity ?tag ?to_ ~name =
        let args = 
            arg_string_opt "-entity" entity ^ 
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name  
        in
        Utils.command_string (buildl "remove_all_parameters" ~args)

    let resolve_path name = 
        Utils.command_string (buildl "resolve_path" ~post:name)

    let revision_name ?project name = 
        let args = arg_string_opt "-project" project in
        Utils.command_string (buildl "revision_name" ~args ~post:name)

    let set_current_revision ?force name = 
        let args = flag_opt "-force" force in
        Utils.command_unit (buildl "set_current_revision" ~args ~post:name)

    let set_global_assignment 
        ?comment ?disable ?entity ?remove 
        ?section_id ?tag ~name value =
        let args = 
            arg_string_opt "-comment" comment ^
            flag_opt "-disable" disable ^  
            arg_string_opt "-entity" entity ^
            flag_opt "-remove" remove ^  
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-tag" tag ^
            " -name " ^ name  
        in
        Utils.command_unit (buildl "set_global_assignment" ~args ~post:value)

    let set_instance_assignment ?comment ?disable 
        ?entity ?fall ?from ~name ?remove ?rise ?section_id 
        ?tag ?to_ value = 
        let args = 
            arg_string_opt "-comment" comment ^
            flag_opt "-disable" disable ^  
            arg_string_opt "-entity" entity ^
            arg_string_opt "-from" from ^
            flag_opt "-remove" remove ^  
            flag_opt "-rise" rise ^  
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name  
        in
        Utils.command_unit (buildl "set_instance_assignment" ~args ~post:value)
            
    let set_io_assignment ?comment ?disable 
        ?io_standard ~name ?remove ?tag value = 
        let args = 
            arg_string_opt "-comment" comment ^
            flag_opt "-disable" disable ^  
            arg_string_opt "-io_standard" io_standard ^
            flag_opt "-remove" remove ^  
            arg_string_opt "-tag" tag ^
            " -name " ^ name  
        in
        Utils.command_unit (buildl "set_io_assignment" ~args ~post:value)

    let set_location_assignment ?comment ?disable 
        ?remove ?tag ~to_ value = 
        let args = 
            arg_string_opt "-comment" comment ^
            flag_opt "-disable" disable ^  
            flag_opt "-remove" remove ^  
            arg_string_opt "-tag" tag ^
            " -to " ^ to_ 
        in
        Utils.command_unit (buildl "set_location_assignment" ~args ~post:value)

    let set_parameter ?comment ?disable 
        ?entity ~name ?remove ?tag ?to_ value = 
        let args = 
            arg_string_opt "-comment" comment ^
            flag_opt "-disable" disable ^  
            arg_string_opt "-entity" entity ^
            flag_opt "-remove" remove ^  
            arg_string_opt "-tag" tag ^
            arg_string_opt "-to" to_ ^
            " -name " ^ name  
        in
        Utils.command_unit (buildl "set_parameter" ~args ~post:value)
            
    let set_power_file_assignment ?remove ?saf_file
        ?section_id ?to_ ?vcd_end_time ?vcd_file ?vcd_start_time = 
        let args = 
            flag_opt "-remove" remove ^
            arg_string_opt "-saf_file" saf_file ^
            arg_int_opt "-section_id" section_id ^
            arg_string_opt "-to" to_ ^
            arg_string_opt "-vcd_end_time" vcd_end_time ^
            arg_string_opt "-vcd_file" vcd_file ^
            arg_string_opt "-vcd_start_time" vcd_start_time 
        in
        Utils.command_unit (buildl "set_power_file_assignment" ~args)

    let set_user_option ~name value = 
        let args = " -name " ^ name in
        Utils.command_unit (buildl "set_user_option" ~args ~post:value)

    let test_assignment_trait ~name ~trait =
        let args = " -name " ^ name ^ " -trait " ^ trait in
        Utils.command_int (buildl "test_assignment_trait" ~args)

end

module Sdc = struct

    type t = [ `DerivePll
             | `DeriveUncertainty
             | `CreateClock of string * float
             | `SetGroup of string ]

    let write_sdc constraints file_name = 
        lwt () = Lwt_io.printf "writing sdc file %s\n" file_name in
        lwt f = Lwt_io.open_file ~mode:Lwt_io.output file_name in
        let write = function
            | `DerivePll -> Lwt_io.fprintf f "derive_pll_clocks\n"
            | `DeriveUncertainty -> Lwt_io.fprintf f "derive_clock_uncertainty\n"
            | `CreateClock(name,period) ->
                Lwt_io.fprintf f "create_clock -period %f %s\n" period name 
            | `SetGroup(name) ->
                Lwt_io.fprintf f "set_clock_groups -exclusive -group %s\n" name 
        in
        lwt () = Lwt_list.iter_s write constraints in 
        Lwt_io.close f 

end

let quartus f = 
    Lwt_process.with_process_full 
        Lwt_process.(shell "quartus_sh -s")
            (fun p -> 
                lwt () = Utils.match_tag (fun _ -> ()) p#stdout in
                lwt r = f p in
                lwt _ = p#close in
                return r)

let run_quartus f = 
    Lwt_main.run (
        match_lwt Utils.which "quartus_sh" with
        | None -> fail (Failure "Can't find quartus_sh.  Check $PATH.")
        | Some(_) -> quartus f
    )

let rec enter_new_dir ?(force=false) dir = 
    match_lwt Utils.direxists dir with
    | true ->
        if force then
            (* if force is set then remove the directory and try again. *) 
            lwt _ = Utils.shell_bool ("rm -fr " ^ dir) in 
            enter_new_dir ~force:false dir
        else 
            fail (Failure ("directory '" ^ dir ^ "' already exists."))
    | false -> 
        lwt () = Utils.mkdir dir in
        Lwt_unix.chdir dir

let new_project ?(force=false) name part f = 
    lwt cwd = Utils.getcwd () in
    try_lwt 
        let dir,project = Filename.dirname name, Filename.basename name in
        (* create the project directory *)
        lwt () = enter_new_dir ~force dir in
        (* create new quartus project *)
        quartus (fun proc ->
            lwt part = Device.report_part_info part proc in
            match_lwt Project.project_new 
                ~family:("{" ^ part.Device.family ^ "}")
                ~part:part.Device.part
                ~overwrite:false
                project proc with
            | true,_ -> (try_lwt f proc finally Project.project_close proc)
            | _ -> fail (Failure "couldn't open project.")
        )
    finally (* return to original directory *)
        Lwt_unix.chdir cwd


let find_simple_clocks circuit = 
    let open HardCaml in
    let open Signal.Types in
    let open Signal.Comb in
    let regs = Circuit.(find_signals is_reg (outputs circuit)) in
    let is_simple_clock r = 
        let s = r.reg_clock in (* unassigned wire ie input *)
        (is_wire s && [empty] = (deps s))
    in
    let set = List.fold_left (fun set -> function
        | Signal_reg(_,r) when is_simple_clock r -> UidSet.add (uid r.reg_clock) set
        | _ -> failwith "find_simple_clocks: not a registger") 
        UidSet.empty regs
    in
    UidSet.fold (fun e l -> e::l) set []

let virtual_pins clocks circuit p = 
    let open HardCaml in
    let vpin s = 
        let name s = List.hd (Signal.Types.names s) in
        if List.mem (Signal.Types.uid s) clocks then 
            (* for USE_CLK_FOR_VIRTUAL_PIN, need CLOCK_SETTINGS *)
            return ()
        else begin
            let w = Signal.Comb.width s in
            if w = 1 then
                Project.set_instance_assignment 
                    ~to_:(name s) ~name:"VIRTUAL_PIN" "ON" p
            else
                for_lwt i=0 to w-1 do
                    Project.set_instance_assignment 
                        ~to_:((name s) ^ "[" ^ string_of_int i ^ "]" )
                        ~name:"VIRTUAL_PIN" "ON" p
                done
        end
    in
    lwt _ = Lwt_list.iter_s vpin (Circuit.inputs circuit) in
    lwt _ = Lwt_list.iter_s vpin (Circuit.outputs circuit) in
    return ()

(* compile a 'general' hardcaml circuit (no IO constraints) *)
let compile_circuit path part circuit = 

    (* write rtl *)
    let name = HardCaml.Circuit.name circuit in
    let full_path = Filename.concat path name in
    let write_hdl () = 
        let file = open_out (name ^ ".v") in
        let _ = HardCaml.Rtl.Verilog.write (output_string file) circuit in
        close_out file
    in
    (* find (simple) clocks *)
    (*let clocks = find_simple_clocks circuit in*)
    
    (* run project *)
    new_project full_path part (fun p ->
        let open Project in
        (* write the verilog file (now the directory exists *)
        let _ = write_hdl () in
        lwt _ = set_global_assignment 
            ~name:"PROJECT_OUTPUT_DIRECTORY" "output_files" p in
        (* add verilog file *)
        lwt _ = set_global_assignment 
            ~name:"VERILOG_FILE" (name ^ ".v") p in
        (* set virtual pins - not valid in web edition *)
        (* lwt _ = virtual_pins clocks circuit p in *)
        (* run flow *)
        lwt _ = Misc.load_package "flow" p in
        lwt _ = Flow.execute_flow `Compile p in
        (* return usage report *)
        lwt _ = Misc.load_package "report" p in
        lwt _ = Report.load_report p in
        lwt report = Report.get_fitter_report p in
        lwt _ = Report.unload_report p in
        (*lwt _ = Utils.command_unit "exit\n" p in*)
        return report
    )

