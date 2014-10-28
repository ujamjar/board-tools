open Utils

module Device : sig

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

    val get_family_list : string list command

    val get_part_info : opts:get_part_info_opts_v list -> string -> string command

    val get_part_list : ?opts:(part_info_v * string) list -> string list command

    val report_device_info : string -> string list command

    val report_family_info : string -> string list command

    val report_part_info : string -> part_info command
    
    val report_part_info_ex : string -> part_info_ex command

    val get_all_parts : part_info list command

    val get_all_parts_ex : part_info_ex list command

end

module Misc : sig

    val checksum : ?algorithm:[ `Crc32 | `Adler32 ] -> filename:string -> string command

    val disable_natural_bus_mapping : unit command
    val enable_natural_bus_mapping : unit command
    
    val escape_brackets : string -> string command
    
    val foreach_in_collection : var:string -> collection:string ->
        body:string -> string list command
    val get_collection_size : collection:string -> int command

    val get_environment_info : [ `Num_logical_processors
                               | `Num_physical_processors
                               | `Operating_system ] -> string command

    val init_tk : unit command

    val load : string list -> unit command

    val load_package : ?version:string -> string -> unit command

    val post_message : 
        ?file:string -> ?line:int ->
        ?typ:[ `Info | `Warning | `Error | `Extra_info | `Critical_warning ] ->
        string -> unit command

    val qexec : string -> string list command

    val qexit : [`Error | `Success] -> unit command

    val stopwatch : [`Lap_time | `Number_format | `Reset | `Start ] ->
        string option command

end

module Flow : sig

    val execute_flow : 
        ?dont_export_assignments:bool ->
        [`Analysis_and_elaboration  
        |`Check_ios  
        |`Check_netlist  
        |`Compile 
        |`Compile_and_simulate  
        |`Create_companion_revision 
        |`Early_timing_estimate  
        |`Eco  
        |`Eda_synthesis  
        |`Export_database 
        |`Fast_model 
        |`Generate_functional_sim_netlist  
        |`Import_database 
        |`Incremental_compilation_export  
        |`Incremental_compilation_import 
        |`Signalprobe 
        |`Vqm_writer] ->
        (bool * string) command

    val execute_hc :
        ?archive:string -> ?compare:bool -> ?create_companion:string ->
        ?handoff_report:bool -> ?hc_ready:bool -> ?min_archive:bool -> 
        (bool * string) command

    val execute_module :
        ?args:string ->
        ?dont_export_assignments:bool ->
        tool:[`Asm|`Cdb|`Drc|`Eda|`Fit|`Map|`Pow|`Sta|`Sim|`Tan|`Si|`Cpf] ->
        (bool * string) command

end

module Report : sig

    type panel_id = [ `Id of int | `Name of string ]
                              
    val add_row_to_table : id:panel_id -> string -> unit command

    val create_report_panel : ?folder:bool -> ?table:bool -> string ->
        unit command

    val delete_report_panel : id:panel_id -> unit command 

    type f_resource = [`Alm|`Alut|`Io_pin|`Lab|`Le|`Mem_bit|`Reg] 
    type f_resource_use = [`Available|`Used|`Percentage]
    type f_resource_full = [f_resource|f_resource_use|`Utilization]

    val get_fitter_resource_usage : f_resource_full list -> string list command
    val get_fitter_resource : f_resource -> f_resource_use -> int command

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

    val get_fitter_report : resources command

    val get_number_of_columns : id:panel_id -> int command 
    val get_number_of_rows : id:panel_id -> int command 

    val create_report_panel : ?folder:bool -> ?table:bool -> string ->
        unit command

    val get_report_panel_column_index : id:panel_id -> string -> int command
    val get_report_panel_row_index : id:panel_id -> string -> int command
    
    val get_report_panel_data : id:panel_id -> col:panel_id -> row:panel_id ->
        string list command

    val get_report_panel_id : string -> int command

    val get_report_panel_names : string command

    val get_report_panel_row : id:panel_id -> row:panel_id ->
        string list command

    val get_timing_analysis_summary_results :
        [`Tsu|`Tco|`Tpd|`Th|`Min_tco|`Min_tpd|`Clock_setup of string|`Clock_hold of string] ->
        [`Slack|`Required|`Actual] ->
        string command

    val load_report : ?simulator:bool -> unit command

    val read_xml_report : string -> unit command

    val save_report_database : unit command
    
    val unload_report : unit command

    val write_report_panel : id:panel_id -> ?html:bool -> ?xml:bool -> file:string -> 
        unit command

    val write_xml_report : string -> unit command

end

module Project : sig

    val assignment_group : 
        ?add_exception:string -> ?add_member:string ->
        ?comment:string -> ?disable:bool -> ?get_exceptions:bool ->
        ?get_members:bool -> ?overwrite:bool -> ?remove:bool ->
        ?remove_exception:string -> ?remove_member:string -> 
        ?tag:string -> string -> unit command

    val create_revision : ?based_on:string ->
        ?copy_results:bool -> ?set_current:bool ->
        string -> unit command

    val delete_revision : string -> unit command

    (*execute_assignment_batch*)

    val export_assignments : ?reorganise:bool -> unit command
    
    val get_all_assignment_names : 
        ?family:string ->
        ?modl:[`All|`Map|`Fit|`Tan|`Asm|`Eda|`Drc|`Generic] ->
        ?typ:[`All|`Global|`Instance] -> 
        string command

    val get_all_assignments : 
        ?entity:string -> ?from:string -> ?section_id:int ->
        ?tag:string -> ?to_:string -> name:string ->
        typ:[`Global|`Instance|`Parameter|`Default] -> string command

    val get_all_global_assignments : 
        ?entity:string -> ?section_id:int ->
        ?tag:string -> name:string ->
        string command

    val get_all_instance_assignments : 
        ?entity:string -> ?from:string -> ?section_id:int ->
        ?tag:string -> ?to_:string -> name:string ->
        string command

    val get_all_parameters : 
        ?entity:string -> ?tag:string -> ?to_:string -> name:string ->
        string command

    val get_all_quartus_defaults : ?section_id:int -> 
        ?name:string -> string command

    val get_all_user_option_names : ?name:string -> string command

    val get_assignment_info : id:int ->
        [`Entity|`From|`Get_tcl_command|`Name|`Section_id|`Tag|`To|`Value] ->
        string command

    val get_current_revision : string -> string command

    val get_global_assignment : 
        ?entity:string -> ?front:bool -> ?section_id:int ->
        ?tag:string -> name:string ->
        string command

    val get_instance_assignment : 
        ?entity:string -> ?from:string -> ?front:bool -> 
        ?section_id:int -> ?tag:string -> ?to_:string ->
        name:string ->
        string command

    val get_location_assignment : ?tag:string -> to_:string -> 
        string command

    type observable_type_v = 
        [`All|`Pre_synthesis|`Post_synthesis|`Post_fitter
        |`Post_asm|`Stp_pre_synthesis] 

    val get_name_info :
        ?get_synonyms:bool -> 
        info:
            [`Parent_name_id|`Base_name|`Entity_name|`Instance_name
            |`Full_path|`Short_full_path|`Node_type|`Creator
            |`Signaltapii|`File_location|`Library|`Children|`Parameters] ->
        ?observable_type:observable_type_v ->
        ?use_cached_database:bool -> int ->
        string command

    val get_names :
        ?entity:string ->
        filter:string ->
        ?library:string ->
        node_type:[`All|`Comb|`Reg|`Pin|`Input|`Output|`Bidir|`Hierarchy|`Mem|`Bus|`Qsf] ->
        ?observable_type:observable_type_v ->
        string command

    val get_parameter : ?entity:string -> name:string -> ?tag:string -> ?to_:string -> 
        string command
    
    val get_project_directory : string command
    
    val get_project_revisions : string -> string command

    val get_top_level_entity : string command

    val get_user_option : string -> string command

    val is_project_open : bool command

    val project_archive : 
        ?all_revisions:bool -> ?include_libraries:bool ->
        ?include_outputs:bool -> ?overwrite:bool ->
        ?use_file_set:string -> ?version_compatible_database:bool ->
        string -> unit command

    val project_close : ?dont_export_assignments:bool -> unit command

    val project_exists : string -> bool command

    val project_new : ?family:string -> ?overwrite:bool -> ?part:string ->
        ?revision:string -> string -> (bool*string) command

    val project_open : 
        ?current_revision:bool -> ?force:bool -> ?revision:string -> 
        string -> (bool*string) command

    val project_restore : ?destination:string -> ?overwrite:bool ->
        ?update_included_file_info:bool -> string -> unit command

    val remove_all_global_assignments : 
        ?entity:string -> ?section_id:int ->
        ?tag:string -> name:string ->
        unit command

    val remove_all_instance_assignments : 
        ?entity:string -> ?from:string -> ?section_id:int ->
        ?tag:string -> ?to_:string -> name:string ->
        string command

    val remove_all_parameters : 
        ?entity:string -> ?tag:string -> ?to_:string -> name:string ->
        string command

    val resolve_path : string -> string command

    val revision_name : ?project:string -> string -> string command

    val set_current_revision : ?force:bool -> string -> unit command

    val set_global_assignment : ?comment:string -> ?disable:bool ->
        ?entity:string -> ?remove:bool -> ?section_id:int ->
        ?tag:string -> name:string -> string -> unit command

    val set_instance_assignment : ?comment:string -> ?disable:bool ->
        ?entity:string -> ?fall:bool -> ?from:string -> name:string ->
        ?remove:bool -> ?rise:bool -> ?section_id:int ->
        ?tag:string -> ?to_:string -> string -> unit command
        
    val set_io_assignment : ?comment:string -> ?disable:bool -> 
        ?io_standard:string -> name:string -> ?remove:bool ->
        ?tag:string -> string -> unit command
    
    val set_location_assignment : ?comment:string -> ?disable:bool -> 
        ?remove:bool -> ?tag:string -> to_:string -> string -> unit command

    val set_parameter : ?comment:string -> ?disable:bool -> ?entity:string ->
        name:string -> ?remove:bool -> ?tag:string -> ?to_:string ->
        string -> unit command


    val set_power_file_assignment : ?remove:bool -> ?saf_file:string ->
        ?section_id:int -> ?to_:string -> ?vcd_end_time:string -> 
        ?vcd_file:string -> ?vcd_start_time:string -> unit command

    val set_user_option : name:string -> string -> unit command

    val test_assignment_trait : name:string -> trait:string -> int command

end

module Sdc : sig
    type t = [ `DerivePll
             | `DeriveUncertainty
             | `CreateClock of string * float
             | `SetGroup of string ]
    val write_sdc : t list -> string -> unit Lwt.t
end

val quartus : 'a command -> 'a Lwt.t
val run_quartus : 'a command -> 'a

val new_project : ?force:bool -> string -> string -> 'a command -> 'a Lwt.t

val compile_circuit : string -> string -> HardCaml.Circuit.t -> 
    Report.resources Lwt.t




