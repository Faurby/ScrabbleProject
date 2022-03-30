// Learn more about F# at http://fsharp.org

open System

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint true // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()


    let board        = ScrabbleUtil.StandardBoard.standardBoard ()
//    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

//    let board      = ScrabbleUtil.RandomBoard.randomBoard ()
//    let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

//    let board      = ScrabbleUtil.HoleBoard.holeBoard ()
//    let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words     = readLines "../../../Dictionaries/English.txt"

    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI =
        // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
        // Some (Dictionary.empty, Dictionary.insert, Dictionary.step, Some Dictionary.reverse) 
        None

    let (dictionary, time) = time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)

    // Uncomment to test your dictionary
    // ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test sucessful\n")
    // let incorrectWords = ScrabbleUtil.Dictionary.test words 10 (dictionary false) // change to true if using a GADDAG
    // match incorrectWords with
    // | [] -> ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test sucessful!\n")
    // | _  ->
    //    ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test failed for at least the following words: \n") 
    //    List.iter (fun str -> ScrabbleUtil.DebugPrint.debugPrint (sprintf "%s\n" str)) incorrectWords
        
    // Uncomment this line to call your client
    // let players    = [("Your name here", dictionary, YourClientName.Scrabble.startGame)]
    let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2
        

    do ScrabbleServer.Comm.startGame 
          board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore

    0
