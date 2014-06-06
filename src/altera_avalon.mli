(* altera/avalon logic *)

open HardCaml
open Signal.Comb
open Signal.Seq

module Avalon : sig

    (* configuration properties *)
    type config = 
        {
            addressUnits : int;
            burstCountUnits : int;
            burstOnBurstBoundariesOnly : bool;
            constantBurstBehaviour : bool;
            holdTime : int;
            linewrapBursts : bool;
            maximumPendingReadTransactions : int;
            readLatency : int;
            readWaitTime : int;
            setupTime : int;
            timingUnits : int;
            writeWaitTime : int;
            associatedClock : string;
            associatedReset : string;
            (*bridgesToMaster:?*)
        }

    val config : config

    (* avalon master driven signals *)
    type 'a master = 
        {
            (* 1..32 bit address *)
            address : 'a;
            begintransfer : 'a;
            (* 1..128 (^2) bit byte enables *)
            byteenable : 'a;
            chipselect : 'a;
            debugaccess : 'a;
            read : 'a;
            write : 'a;
            (* 8..1024 (^2) bit data bus *)
            writedata : 'a;
            lock : 'a;
            (* 1..11 *)
            burstcount : 'a;
            beginbursttransfer : 'a;
        }
    
    val master : t master

    val map_master : (string -> 'a -> 'b) -> 'a master -> 'b master

    val list_master : 'a master -> 'a list

    (* avalon slave driven signals *)
    type 'a slave = 
        {
            (* 8..1024 (^2) bit data bus *)
            readdata : 'a;
            waitrequest : 'a;
            readdatavalid: 'a;
        }

    val slave : t slave

    val map_slave : (string -> 'a -> 'b) -> 'a slave -> 'b slave

    val list_slave : 'a slave -> 'a list

end

module MakeAvalonMM(Seq : Seqgen.S) : sig

    val slave_regs : 
        Avalon.config -> t Avalon.master -> 
        Regmap.reg_spec list -> t list * t Avalon.slave

end

