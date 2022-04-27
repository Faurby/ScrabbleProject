// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"
    let pIsConsonant = pstring "isConsonant"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let curlybracket p = pchar '{' >*>. p .>*> pchar '}'

    let charListToStr (a: char list) = System.String.Concat(a)

    let pid = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> fun (a, b) -> charListToStr(a::b)

    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul ((N -1), a)) <?> "Neg"
    let VParse = pid |>> V <?> "V"

    let AexpParse = TermParse 

    let CParse, cref = createParserForwardedToRef<cExp>()

    let charParse = between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "C"
    let toUppperParse = unop pToUpper (parenthesise CParse) |>> ToUpper <?> "ToUpper"
    let toLowerParse = unop pToLower (parenthesise CParse) |>> ToLower <?> "ToLower"
    let intToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"
    let charValueParse = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CV"
    do cref := choice [charValueParse; intToCharParse; toUppperParse; toLowerParse; charParse]

    let CharToIntParser = unop pCharToInt (parenthesise CParse) |>> CharToInt <?> "V"
    do aref := choice [CharToIntParser; NegParse; PVParse; VParse; NParse; ParParse]
    
    let CexpParse = CParse
    
    let BTerm, btref = createParserForwardedToRef<bExp>()
    let BProd, bpref = createParserForwardedToRef<bExp>()
    let BAtom, baref = createParserForwardedToRef<bExp>()

    let andParse = binop (pstring "/\\") BProd BTerm |>> Conj <?> "Conj"
    let orParse = binop (pstring "\\/") BProd BTerm |>> (fun (x, y) -> Not (Conj (Not x, Not y))) <?> "Conj"

    do btref := choice [andParse; orParse; BProd]

    let equalParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "AEq"
    let notEqual = binop (pstring "<>") AexpParse AexpParse |>> (fun (x, y) -> x .<>. y) <?> "Not Equal"
    let lessThanParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "ALt"
    let biggerThanOrEqualParse = binop (pstring ">=") AexpParse AexpParse |>> (fun (x, y) -> x .>=. y) <?> "BiggerOrEqual"
    let biggerThan = binop (pchar '>') AexpParse AexpParse |>> (fun (x,y) -> x .>. y) <?> "Great than"
    let lessOrEqual = binop (pstring "<=") AexpParse AexpParse |>> (fun (x,y) -> x .<=. y) <?> "lessOrEqual"
    do bpref := choice [equalParse; notEqual; lessThanParse; lessOrEqual; biggerThan; biggerThanOrEqualParse; BAtom]

    let trueParse = pTrue |>> (fun _ -> TT) <?> "True"
    let falseParse = pFalse |>> (fun _ -> FF) <?> "False"
    let notParse = unop (pchar '~') BAtom |>> (fun x -> Not x) <?> "Not"
    let isVowel = unop (pIsVowel) CexpParse |>> IsVowel <?> "IsVowel"
    let isConsonant = unop (pIsConsonant) CexpParse |>> IsConsonant <?> "IsConsonant"
    // let isLetterPrase = unop (pIsLetter) CexpParse |>> IsLetter <?> "IsLetter"
    // let isDigit = unop (pIsDigit) CexpParse |>> IsDigit <?> "IsDigit"
    let parParse = parenthesise BTerm
    do baref := choice [notParse; isVowel; isConsonant; trueParse; falseParse; parParse]

    let BexpParse = BTerm

    let SParse, sref = createParserForwardedToRef<stm>()

    let assignParse = binop (pstring ":=") (spaces >*>. pid) (spaces >*>. AexpParse) |>> Ass <?> "Assign"
    let declareParse = unop pdeclare (spaces1 >*>. pid) |>> Declare <?> "Declare"
    // let seqParse = binop (pchar ';') (spaces >*>. SParse) (spaces >*>. SParse) |>> Seq <?> "Seq"
    
    do sref := choice [assignParse; declareParse]
    let stmntParse = SParse

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

    type boardFun2 = coord -> Result<square option, Error>    

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {
        center = (0,0); 
        defaultSquare = Map.empty; 
        squares = fun _ -> Success (Some Map.empty)
    }
