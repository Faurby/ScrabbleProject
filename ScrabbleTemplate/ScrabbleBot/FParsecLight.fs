module internal FParsecLight

    open FParsec

    type ParserResult<'a> = ParserResult<'a, unit>

    module TextParser =

        type Parser<'a> = Parser<'a, unit>

        let satisfy f : Parser<char> = satisfy f

        let (<?>) (p : Parser<'a>) l : Parser<'a> = p <?> l

        let (.>>.) (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> = p1 .>>. p2
        let (.>>)  (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a> = p1 .>> p2
        let (>>.)  (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'b> = p1 >>. p2

        let (|>>) (p : Parser<'a>) (f : 'a -> 'b) : Parser<'b> = p |>> f

        let (<|>)  (p : Parser<'a>) q : Parser<'a> = attempt p <|> q 
        let choice (ps : Parser<'a> seq) : Parser<'a> = choice (Seq.map attempt ps) 

        let many  (p : Parser<'a>) : Parser<'a list> = many p
        let many1 (p : Parser<'a>) : Parser<'a list> = many1 p

        let opt     (p : Parser<'a>) : Parser<'a option> = opt p
        let between l r (p : Parser<'c>) : Parser<'c> = between l r p

        let sepBy  (p : Parser<'a>) sep : (Parser<'a list>) = sepBy p sep
        let sepBy1 (p : Parser<'a>) sep : (Parser<'a list>) = sepBy1 p sep

        let anyChar     : Parser<char> = anyChar
        let asciiLetter : Parser<char> = asciiLetter
        let digit       : Parser<char> = digit

        let pchar   c : Parser<char>   = pchar c
        let pstring s : Parser<string> = pstring s
        let pint32    : Parser<int>  = pint32

        let createParserForwardedToRef () : Parser<'a> * Parser<'a> ref = createParserForwardedToRef ()

        let run (p : Parser<'a>) str : ParserResult<'a> = run p str

        let getSuccess : ParserResult<'a> -> 'a =
            function
            | Success(s, _, _) -> s
            | failure          -> failwith (sprintf "%A" failure)