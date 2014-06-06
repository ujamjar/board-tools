(*
#use "topfind"
#require "ujamjar"
open UJamJar
*)

open HardCaml
open Signal.Comb

module Avalon = struct

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

    let config = 
        {
            addressUnits=0;
            burstCountUnits=0;
            burstOnBurstBoundariesOnly=false;
            constantBurstBehaviour=false;
            holdTime=0;
            linewrapBursts=false;
            maximumPendingReadTransactions=1;
            readLatency=0;
            readWaitTime=1;
            setupTime=0;
            timingUnits=0;
            writeWaitTime=0;
            associatedClock="";
            associatedReset=""; 
        }

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

    let master = 
        {
            address=empty;
            begintransfer=empty;
            byteenable=empty;
            chipselect=empty;
            debugaccess=empty;
            read=empty;
            write=empty;
            writedata=empty;
            lock=empty;
            burstcount=empty;
            beginbursttransfer=empty;
        }

    let map_master f m = 
        {
            address            = f "address" m.address;
            begintransfer      = f "begintransfer" m.begintransfer;
            byteenable         = f "byteenable" m.byteenable;
            chipselect         = f "chipselect" m.chipselect;
            debugaccess        = f "debugaccess" m.debugaccess;
            read               = f "read" m.read;
            write              = f "write" m.write;
            writedata          = f "writedata" m.writedata;
            lock               = f "lock" m.lock;
            burstcount         = f "burstcount" m.burstcount;
            beginbursttransfer = f "beginbursttransfer" m.beginbursttransfer;
        }

    let list_master m = 
        [
            m.address;
            m.begintransfer;
            m.byteenable;
            m.chipselect;
            m.debugaccess;
            m.read;
            m.write;
            m.writedata;
            m.lock;
            m.burstcount;
            m.beginbursttransfer;
        ]

    (* avalon slave driven signals *)
    type 'a slave = 
        {
            (* 8..1024 (^2) bit data bus *)
            readdata : 'a;
            waitrequest : 'a;
            readdatavalid: 'a;
        }

    let slave = 
        { 
            readdata=empty; 
            waitrequest=empty; 
            readdatavalid=empty;
        }

    let map_slave f s = 
        {
            readdata = f "readdata" s.readdata;
            waitrequest = f "waitrequest" s.waitrequest;
            readdatavalid = f "readdatavalid" s.readdatavalid;
        }

    let list_slave s =
        [
            s.readdata;
            s.waitrequest;
            s.readdatavalid;
        ]

end

module MakeAvalonMM(Seq : Seqgen.S) = struct
    open Seq
    open Avalon

    let resize_mux l = 
        let max = List.fold_left (fun m s -> max m (width s)) 0 l in
        List.map (fun s -> uresize s max) l

    (* slave read/write register bank *)
    let slave_regs config master regs = 
        (* pipeline write signals *)
        let write  = Seq.pipeline config.writeWaitTime vdd master.write in
        let byteenable = Seq.pipeline config.writeWaitTime vdd master.byteenable in
        let writedata = Seq.pipeline config.writeWaitTime vdd master.writedata in
        (* register bank *)
        let qcore = (List.map (fun r -> r write byteenable writedata) regs) in
        let q = mux master.address (resize_mux qcore) in
        (* pipe line read signal *)
        let q = Seq.pipeline config.readLatency vdd q in
        let pipe1 = Seq.pipeline ~cv:vdd
            (*~m:(Signal.Types.(fun m -> {m with reg_clear_value=vdd}))*)
        in
        (* create slave interface *)
        let waitread = pipe1 config.readLatency vdd (~: (master.read)) in
        let waitwrite = pipe1 config.writeWaitTime vdd (~: (master.write)) in
        qcore,
        { Avalon.slave with readdata=q; waitrequest=waitread &: waitwrite }

    (* slave FIFO *)


    (* slave RAM *)


    (* master FIFO - programmed to transfer N words to some address via a slave
     * interface *)

    (* master RAM - similar to the FIFO *)

end

(*
module Regs = Seqgen.MakeSeq(struct
    let reg_spec = Signal.Seq.r_sync
    let ram_spec = Signal.Seq.r_none
end)
*)

