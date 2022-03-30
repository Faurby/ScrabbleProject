// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a> = Temp of unit // Not implemented

    let empty : MultiSet<'a> = Temp () // Not implemented
    let add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun _ _ _ -> failwith "Not implemented" 
    let fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun _ _ _ -> failwith "Not implemented"
