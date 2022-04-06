// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

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
       | IsConsonant of cExp  (* check for consonant *)

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
        // | IsDigit x -> charEval x >>= (fun a -> ret (System.Char.IsDigit a))
        // | IsLetter x -> charEval x >>= (fun a -> ret (System.Char.IsLetter a))
        | IsVowel x -> charEval x >>= (fun a -> ret ("aeuioæøå".Contains(System.Char.ToLower a)))
        | IsConsonant x -> charEval x >>= (fun a -> ret ("bcdfghjklmnpqrstvwxyz".Contains(System.Char.ToLower a)))


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"