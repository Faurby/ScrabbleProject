namespace LowOrbitScrabbleCannon

open Microsoft.VisualBasic
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =
    open MultiSet  
    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ indexOfLetter letterCount -> debugPrint (sprintf "%d -> (%A, %d)\n" indexOfLetter (Map.find indexOfLetter pieces) letterCount)) ()
    
    // char number to points (borrowed from https://github.com/4lgn/scrabble-bot/blob/master/ScrabbleBot/Helpers.fs)
    let charNumberToPoints (ch: int) = 
            match ch with
            | 0                                             -> 0
            | 1 | 5 | 9 | 12 | 14 | 15 | 18 | 19 | 20 | 21  -> 1
            | 4 | 7                                         -> 2
            | 2 | 3 | 13 | 16                               -> 3
            | 6 | 8 | 22 | 23 | 25                          -> 4
            | 11                                            -> 5
            | 10 | 24                                       -> 8
            | 17 | 26                                       -> 10
            | _                                             -> failwith "Not valid character index"

    let uintToChar id = char(id + 64u)

    // char to uint (borrowed from https://github.com/4lgn/scrabble-bot/blob/master/ScrabbleBot/Helpers.fs)
    let charToUint ch = 
        if (ch = '?') then 0u
        else uint32(System.Char.ToUpper(ch)) - 64u

    // Given a hand, return list of chars
    let handToChar hand =
        MultiSet.toList hand |> List.map uintToChar
    
    // Given a word, convert to list of moves
    let wordToMove (word:string) (startingCoord:coord) (direction: (int*int)) (playedLetters: Map<coord, (char * int)>) =
        let aux (word:string) (startingCoord:coord) (direction: (int*int)) (playedLetters: Map<coord, (char * int)>) =
            List.fold (fun (acc:(list<(int*int) * (uint32 * (char * int))> * int)) char ->
            let (directionRight, directionDown) = direction
            let (x, y) = startingCoord
            let index = (snd acc)
            let listOfMoves = (fst acc)

            let coordToPlaceLetter = (x + (index * directionRight), (y + (index * directionDown)))
            let charnumber = charToUint char

            // Print coordtoplaceletter
            //debugPrint (sprintf "CoordToPlaceLetter: %d, %d\n" (fst coordToPlaceLetter) (snd coordToPlaceLetter))

            match playedLetters.TryGetValue coordToPlaceLetter with
            | (true, _) -> (listOfMoves, index+1)
            | (false, _) -> (coordToPlaceLetter, (charnumber,(char, charNumberToPoints (int charnumber))))::listOfMoves, index+1
            ) ([], 0) (word |> Seq.toList)
        fst (aux word startingCoord direction playedLetters)

    // Convert multiset<uint32> to multiset<char>
    let multisetToChar = MultiSet.map (fun i -> uintToChar i) 

    let rec getLongestWordFirstMove (hand : MultiSet<uint32>) (accCurrentString : string) (dict : Dict) (playedLetters: Map<coord, (char * int)>) (coord:coord) (direction:(int * int)) =

        // First determine what exists at the coordinate of the letter we are writing. If there is already a letter we must weave
        // This into the word we are writing rather than using letters from our hand
        let existingLetter = Map.tryFind coord playedLetters
        
        // This is what we will be building the word with. (What we fold over)
        // If there is not already a letter on the board we use the letters in our hand
        // If there is a letter on the board we use this letter instead of our hand. This lets us
        // weave in existing letters on the board to the word we are constructing
        let wordBuildingBlock =
            match existingLetter with
            | Some value -> MultiSet.toList (MultiSet.addSingle (fst value) MultiSet.empty)
            | None _ -> MultiSet.toList (multisetToChar hand)
        
        //debugPrint (sprintf "In getting longest word: \n Word so far: %A\n hand: %A\n\n" accCurrentString hand)
        
        // Fold over our world building blocks
        List.fold (fun (longestWordSoFar:string) letter ->
            
            // Get The string we are looking at now within List.fold
            let currentString = (accCurrentString + (string) letter)
            
            // Get child node dictionary
            let childNode = Dictionary.step letter dict
          
            // Check child node to see if we have hit the end of a word
            match childNode with
            | Some (currentStringIsWord, nodeDict) ->               
                // If the letter does not already exist on the board we remove it from our hand
                let newHand = match existingLetter with
                | Some _ -> hand
                | None _ -> (MultiSet.removeSingle (charToUint letter) hand)
                
                // Figure out the coord based on the direction and last coord
                let newCoord = (fst(coord) + fst(direction), snd(coord) + snd(direction))
                
                // Recursively piece together all words
                let newPotentialWord = getLongestWordFirstMove newHand currentString (nodeDict)  (playedLetters: Map<coord, (char * int)>) newCoord direction
                
                if currentStringIsWord && currentString.Length > newPotentialWord.Length && currentString.Length > longestWordSoFar.Length then
                    currentString
                elif newPotentialWord.Length > longestWordSoFar.Length then
                    newPotentialWord
                else
                    longestWordSoFar
            | None _ -> longestWordSoFar
            ) "" wordBuildingBlock
    
    let getLongestWordContinuation (hand : MultiSet<uint32>) (accCurrentString: string) (dict: Dict) (playedLetters: Map<coord, (char * int)>) (coord:coord) (direction:(int * int))=
        // Get dict from word so far
        let (wordLength, wordDictSoFar) =
            List.fold (fun (depth:int, dict:Dict) letter ->
                let childNode = Dictionary.step letter dict
                match childNode with
                | Some (_, nodeDict) ->
                    (depth + 1, nodeDict)
            ) (0,dict) (accCurrentString |> Seq.toList)
        getLongestWordFirstMove hand accCurrentString wordDictSoFar playedLetters ((fst coord) + ((fst direction) * wordLength),(snd coord) + ((snd direction) * wordLength)) direction  
    
module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        numberOfPlayers: uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn    : uint32
        playedLetters : Map<coord, (char * int)>
    }

    let mkState newBoard newDict newPlayernumber newNumberOfPlayers newHand newPlayerTurn playedLetters= 
        {
            board = newBoard; 
            dict = newDict;  
            playerNumber = newPlayernumber; 
            numberOfPlayers = newNumberOfPlayers; 
            hand = newHand
            playerTurn = newPlayerTurn
            playedLetters = playedLetters
        }

    let board st           = st.board
    let dict st            = st.dict
    let playerNumber st    = st.playerNumber
    let numberOfPlayers st = st.numberOfPlayers
    let hand st            = st.hand
    let playerTurn st      = st.playerTurn
    let playedLetters st   = st.playedLetters

    // Given a list of moves, insert played letters into the state (playedLetters)
    let insertMovesIntoState (moves:list<coord * (uint32 * (char * int))>) (state:state) =
        List.fold (fun acc move ->
            let (coord, (_,(char, charPoints))) = move
            //debugPrint (sprintf "Inserting move %A %A\n" coord (char))
            let newPlayedLetters = acc.playedLetters |> Map.add coord (char, charPoints)
            mkState acc.board acc.dict acc.playerNumber acc.numberOfPlayers acc.hand acc.playerTurn newPlayedLetters
        ) state moves 
    
    // Recusively move to the next and when we hit an empty square, return the word.
    // If letter has non-empty square over or to the left, return.
    let getExistingWord (coord:coord) (direction:(int * int)) (playedLetters) =
        let rec aux (coord:coord) (direction:(int * int)) (playedLetters) acc =
            let newCoord = (fst(coord) + fst(direction), snd(coord) + snd(direction))
            match Map.tryFind newCoord playedLetters with
            | None -> acc
            | Some (letter, _) -> aux newCoord direction playedLetters (acc + string letter)
        
        // Early opt-out
        let checkPrevious = (fst(coord) - fst(direction), snd(coord) - snd(direction))
        match Map.tryFind checkPrevious playedLetters with
        | None -> Some (aux coord direction playedLetters (string (fst(Map.find coord playedLetters))))
        | Some a -> None

    let directionalWordLookup playedLetters dir =
        Map.fold (fun (acc:List<(string * (coord * (int * int)))>) (coord:coord) ((letter:char), _) -> 
            // See if there already exists a downward word on these coordinates
            let existingWordDown = getExistingWord coord dir playedLetters
            match existingWordDown with
            | Some a -> (a, (coord, dir)) :: acc
            | None -> acc
            ) [] playedLetters

    // Returns map of starters
    let wordLookup playedLetters = 
        let downWords = directionalWordLookup playedLetters (0,1)
        let rightWords = directionalWordLookup playedLetters (1,0)
        
        // Merge the two lists
        downWords@rightWords

module Scrabble =
    open System.Threading  
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) (myTurn: bool) =
            debugPrint("\n\n@@@@@@@@@@@@@@@@\naux call started\n@@@@@@@@@@@@@@@\n\n")

            if (myTurn) then
                debugPrint("\n=======================\n**** My Turn ****\n=======================\n")
                debugPrint(sprintf "**** My hand: ****\n" )
                Print.printHand pieces (State.hand st)
               
                if st.playedLetters.Count = 0 then
                    // First move
                    debugPrint("**** Playing the first word of the game ****\n")

                    // Find the longest word we can play on our hand
                    let longestWord = Print.getLongestWordFirstMove (State.hand st) "" st.dict st.playedLetters (0,0) (1,0)
                    
                    debugPrint(sprintf "**** Playing word: %A ****\n" longestWord )
                    
                    let move = Print.wordToMove longestWord (0,0) (1, 0) (State.playedLetters st)
                    send cstream (SMPlay move)
                else
                    // Continuing move
                    debugPrint("**** Playing move continuing off what is currently on the board ****\n")
                    
                    //TODO: replace starters map with list. Right now we will overwrite any duplicate starters.
                    // Find all words already on teh board
                    let starters = State.wordLookup st.playedLetters

                    let listOfAllWordsWeCanPlay =
                        List.fold (fun (acc:list<string * (coord * (int * int))>) (key,value) ->
                            let (coord, dir) = value
                            let longestWord = Print.getLongestWordContinuation (State.hand st) key (State.dict st) st.playedLetters coord dir
                            if longestWord.Length > 0 then
                                (longestWord, value)::acc
                            else
                                acc
                        ) [] starters
                        
                    debugPrint(sprintf "**** List of all words we can play: ****\n%A\n" listOfAllWordsWeCanPlay)
                    
                    // For each word, insert it in the state, and check that every word longer than 1 character in the state is in the dictionary
                    let longestWordWeCanPlay =
                        List.fold (fun (acc:string * (coord * (int * int))) (word:string * (coord * (int * int))) -> 
                            // Convert the word to a move
                            let move = Print.wordToMove (fst word) (fst (snd word)) (snd (snd word)) (State.playedLetters st)
                            // Insert the new word into a temporary state that we can check to see if the move is legal
                            let stateWithInsertedMove = State.insertMovesIntoState move st
                            // Get list of every word in the new state
                            let everyWordOnTheBoardInStateWithInsertedMove = State.wordLookup stateWithInsertedMove.playedLetters
                            
                            //TODO: We can skip all of this if the word is shorter than our longest word we've and validated found so far
                            // Check to see if every word is in the dictionary. If they are not we do not consider the state valid                                                               
                            let stateValid =
                                List.fold (fun (stateValidity:bool) (key:string, _) ->
                                if stateValidity then
                                    if key.Length = 1 then
                                        true
                                    elif Dictionary.lookup key st.dict then
                                        true
                                    else
                                        false
                                else
                                    false
                                ) true everyWordOnTheBoardInStateWithInsertedMove
                            
                            // Replace our current longest word if word is longer and we have
                            // confirmed that every word on the board is in the dictionary
                            if stateValid && ((fst word).Length > (fst acc).Length)
                                then word
                            else
                                acc
                                          
                        ) ("", ((0, 0), (0,0))) listOfAllWordsWeCanPlay
                   
                    //let longestWordWeCanPlay =
                    //    List.fold (fun (acc:string * (coord * (int * int))) (elem:string * (coord * (int * int))) -> 
                    //        if ((fst elem).Length > (fst acc).Length)
                    //            then elem
                    //        else
                    //            acc
                    //    ) ("", ((0, 0), (0,0))) listOfAllWordsWeCanPlay

                    debugPrint (sprintf "\n\n======== Longest word we can play =========\n%A\n" (fst longestWordWeCanPlay))
                    
                    let move = Print.wordToMove (fst longestWordWeCanPlay) (fst (snd longestWordWeCanPlay)) (snd (snd longestWordWeCanPlay)) (State.playedLetters st)
                    send cstream (SMPlay move)
            else
                debugPrint("\n=======================\n**** OPPONENT TURN ****\n=======================\n")
        
            //Print.printHand pieces (State.hand st)

            // What we send to server
            // debugPrint (sprintf ":)I Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            // What we receive from server
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(moves, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint (sprintf "============ Successful play by you. ============\n")
                // Update playedLetters with new moves
                let updatedStateLetters = State.insertMovesIntoState moves st

                // get a multiset of the indexes (uint) of the tiles you played
                let playedIndexes = moves |> Seq.map (fun m -> fst (snd m)) |> Seq.toList |> MultiSet.ofList

                // remove played tiles from your hand
                let subtractedHand = MultiSet.subtract (State.hand st) playedIndexes

                // add the new tiles to your hand
                let newHand = List.fold (fun acc (indexOfLetter, letterCount) -> 
                    MultiSet.add indexOfLetter letterCount acc) subtractedHand newPieces

                // Update the state
                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberOfPlayers st) newHand (State.playerTurn st) updatedStateLetters.playedLetters
                
                aux newState false
            | RCM (CMPlayed (pid, moves, points)) ->
                (* Successful play by other player. Update your state *)
                debugPrint (sprintf "============ CMPlayed ============\n")

                // Update playedLetters with new moves
                let updatedStateLetters = State.insertMovesIntoState moves st

                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberOfPlayers st) (State.hand st) (State.playerTurn st) updatedStateLetters.playedLetters
                aux newState (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM (CMPlayFailed (pid, moves)) ->
                debugPrint (sprintf "============ CMPlayFailed ============\n")
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st' (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st false

        aux st (st.playerTurn = st.playerNumber)
        
    // let getLongestWord (cList : char List) =
    //    let rec getLongestWordAux (cList : char List) (currentWord : string) =
    //        function
    //        | Leaf (true) -> currentWord
    //        | Node (_, dic) ->
    //            List.fold (fun acc ele -> 
    //            match dic.TryGetValue ele with
    //            | (true, value) -> 
    //                let newWord = getLongestWordAux (List.filter (fun x -> x <> ele) cList) (currentWord + (string) ele) (value)
    //                if newWord.Length > acc.Length then newWord
    //                else acc
    //            | (false, _) -> acc
    //            ) "" cList
    //        | Leaf (false) -> "Something went wrong..."
    //    getLongestWordAux cList ""
    
    
    
        
    // For each letter in hand
        // step function for each letter.
        
    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers handSet playerTurn Map.empty)
        