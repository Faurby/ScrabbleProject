module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let add (a: SM<int>) (b: SM<int>) = a >>= (fun x -> b >>= (fun y -> ret (x+y)))
    let sub (a: SM<int>) (b: SM<int>) = a >>= (fun x -> b >>= (fun y -> ret (x-y)))
    let mul (a: SM<int>) (b: SM<int>) = a >>= (fun x -> b >>= (fun y -> ret (x*y)))
    let modu a b = a >>= (fun x -> b >>= (fun y -> if (y = 0) then fail (DivisionByZero) else ret (x%y)))
    let aeq a b = a >>= (fun x -> b >>= (fun y -> ret (x=y)))
    let alt a b = a >>= (fun x -> b >>= (fun y -> ret (x<y)))
    let conj a b = a >>= (fun x -> b >>= (fun y -> ret (x&&y)))
    let div a b = a >>= (fun x -> b >>= (fun y -> if (y = 0) then fail (DivisionByZero) else ret (x/y)))   

    let rec arithEval (a: aExp) : SM<int> =
        match a with
        | WL -> wordLength
        | PV x -> (arithEval x) >>= pointValue
        | V x -> lookup x
        | Add (x, y) -> add (arithEval x) (arithEval y)
        | Sub (x, y) -> sub (arithEval x) (arithEval y)
        | Mul (x, y) -> mul (arithEval x) (arithEval y)
        | Mod (x, y) -> modu (arithEval x) (arithEval y)
        | Div (x, y) -> div (arithEval x) (arithEval y)
        | N x -> ret x
        | CharToInt x -> charEval x >>= (fun a -> ret(int a))
    and charEval (c: cExp) : SM<char> = 
        match c with
        | C x -> ret x  
        | CV x -> (arithEval x) >>= characterValue 
        | ToUpper x -> charEval x >>= (fun a -> ret (System.Char.ToUpper a))
        | ToLower x -> charEval x >>= (fun a -> ret (System.Char.ToLower a))
        | IntToChar x -> arithEval x >>= (fun a -> ret (char a))
    and boolEval (b: bExp) : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (x, y) -> aeq (arithEval x) (arithEval y)
        | ALt (x, y) -> alt (arithEval x) (arithEval y)
        | Not x -> boolEval x >>= (fun a -> ret (not a))
        | Conj (x, y) -> conj (boolEval x) (boolEval y)
        | IsDigit x -> charEval x >>= (fun a -> ret (System.Char.IsDigit a))
        | IsLetter x -> charEval x >>= (fun a -> ret (System.Char.IsLetter a))
        | IsVowel x -> charEval x >>= (fun a -> ret ("aeuioæøå".Contains(System.Char.ToLower a)))


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare x -> declare x
        | Ass (x, y) -> ((arithEval y) >>= (fun a -> update x a))
        | Skip -> ret ()
        | Seq (x, y) -> stmntEval x >>>= stmntEval y
        | ITE (x, y, z) -> push >>>= (boolEval x >>= (fun a -> if a then stmntEval y else stmntEval z))
        | While (x, y) -> push >>>= (boolEval x >>= (fun a -> if a then stmntEval y >>>= stmntEval (While (x, y)) else ret ()))

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let rec arithEval2 (a: aExp) : SM<int> =
        prog {
            match a with
            | WL ->
                let! wl = wordLength
                return wl
            | PV x ->
                let! value = arithEval2 x
                let! result = pointValue value
                return result
            | V x ->
                let! result = lookup x
                return result
            | Add (x, y) ->
                let! result = add (arithEval2 x) (arithEval2 y)
                return result
            | Sub (x, y) ->
                let! result = sub (arithEval2 x) (arithEval2 y)
                return result
            | Mul (x, y) ->
                let! result = mul (arithEval2 x) (arithEval2 y)
                return result
            | Mod (x, y) ->
                let! result = modu (arithEval2 x) (arithEval2 y)
                return result
            | Div (x, y) ->
                let! result = div (arithEval2 x) (arithEval2 y)
                return result
            | N x ->
                return x
            | CharToInt x ->
                let! result = charEval x
                return int result
        }
    let rec charEval2  (c: cExp) : SM<char> =
        prog {
            match c with
            | C x ->
                return x  
            | CV x ->
                let! value = arithEval2 x
                let! result = characterValue value
                return result
            | ToUpper x ->
                let! value = charEval2 x
                return System.Char.ToUpper value
            | ToLower x ->
                let! value = charEval2 x
                return System.Char.ToLower value
            | IntToChar x ->
                let! value = arithEval2 x
                return char value
        }
    let rec boolEval2 (b: bExp) : SM<bool> =
        prog {
            match b with
            | TT ->
                return true
            | FF ->
                return false
            | AEq (x, y) ->
                let! result = aeq (arithEval2 x) (arithEval2 y)
                return result
            | ALt (x, y) ->
                let! result = alt (arithEval2 x) (arithEval2 y)
                return result
            | Not x ->
                let! value = boolEval2 x
                return not value
            | Conj (x, y) ->
                let! result = conj (boolEval2 x) (boolEval2 y)
                return result
            | IsDigit x ->
                let! value = charEval x
                return System.Char.IsDigit value
            | IsLetter x ->
                let! value = charEval x
                return System.Char.IsLetter value
            | IsVowel x ->
                let! value = charEval x
                return "aeuioæøå".Contains(System.Char.ToLower value)
        }

    let rec stmntEval2 stmnt : SM<unit> =
        prog {
            match stmnt with
            | Declare x ->
                do! declare x
            | Ass (x, y) ->
                let! value = arithEval2 y
                do! update x value
            | Skip ->
                return ()
            | Seq (x, y) ->
                do! stmntEval x
                do! stmntEval y
            | ITE (x, y, z) ->
                do! push
                let! eval = boolEval2 x
                if eval then
                    do! stmntEval2 y
                else
                    do! stmntEval2 z
            | While (x, y) ->
                do! push
                let! eval = boolEval x
                if eval then
                    do! stmntEval2 y
                    do! stmntEval2 (While (x, y))
                else
                    return ()
        }

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm (w:word) (pos:int) (acc:int) =
        let state = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]
        stmntEval stm >>>= lookup "_result_" |> evalSM state

    let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
    let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithDoubleWordScore = N 2 .*. V "_acc_"
    let arithTripleWordScore = N 3 .*. V "_acc_"
    let stmntSingleLetterScore = Ass ("_result_", arithSingleLetterScore)
    let stmntDoubleLetterScore = Ass ("_result_", arithDoubleLetterScore)
    let stmntTripleLetterScore = Ass ("_result_", arithTripleLetterScore)
    let stmntDoubleWordScore = Ass ("_result_", arithDoubleWordScore)
    let stmntTripleWordScore = Ass ("_result_", arithTripleWordScore)
    let singleLetterScore = stmntToSquareFun stmntSingleLetterScore
    let doubleLetterScore = stmntToSquareFun stmntDoubleLetterScore
    let tripleLetterScore = stmntToSquareFun stmntTripleLetterScore
    let doubleWordScore = stmntToSquareFun stmntDoubleWordScore
    let tripleWordScore = stmntToSquareFun stmntTripleWordScore

    let containsNumbers =
     stmntToSquareFun
     (Seq (Declare "i",
     (Seq (Ass ("_result_", V "_acc_"),
     While (V "i" .<. WL,
     ITE (IsDigit (CV (V "i")),
     Seq (
     Ass ("_result_", V "_result_" .*. N -1),
     Ass ("i", WL)),
     Ass ("i", V "i" .+. N 1)))))))

    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error>
    let stmntToBoardFun stm (map:Map<int, 'a>) (coord:coord) =
        let state = mkState [("_x_", fst coord); ("_y_", snd coord); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"]
        stmntEval stm >>>= lookup "_result_" >>= (fun a -> ret (map.TryFind a)) |> evalSM state

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }