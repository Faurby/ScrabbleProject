module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    // Exercise 6.1
    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = s.vars.Tail}))

    // Exercise 6.2
    let wordLength : SM<int> = 
        S (fun s -> Success (s.word.Length, s))  

    let characterValue (pos : int) : SM<char> =
        S (fun s -> if pos >= s.word.Length || pos < 0
                    then Failure (IndexOutOfBounds pos) 
                    else Success ((fst s.word.[pos]), s)) 

    let pointValue (pos : int) : SM<int> =
        S (fun s -> if pos >= s.word.Length || pos < 0 
                    then Failure (IndexOutOfBounds pos) 
                    else Success ((snd s.word.[pos]) , s))      
    
    let update (var : string) (value : int) : SM<unit> =
        let rec aux (listBefore:List<Map<string, int>>) =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind var m with
                | Some v -> 
                    Some (listBefore@(Map.add var value m):: ms)
                | None   -> aux (m::listBefore) ms

        S (fun s -> 
              match aux (List.empty<Map<string, int>>) (s.vars) with
              | Some v -> Success ((), {s with vars = v})
              | None   -> Failure (VarNotFound var)) 
              
    let declare (var : string) : SM<unit> =
        let rec aux =
            function
            | []      -> None
            | m :: ms -> Map.tryFind var m
        S (fun s -> 
                if (s.reserved.Contains(var)) then Failure (ReservedName var)
                else
                    match aux (s.vars) with
                    | Some _ -> Failure (VarExists var)
                    | None   -> Success ((), {s with vars = (Map.add var 0 (s.vars.Head)::s.vars.Tail)}))

    