namespace LowOrbitScrabbleCannon

open Microsoft.VisualBasic
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication
open Utility

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
        timeout       : uint32 option
    }

    let mkState newBoard newDict newPlayernumber newNumberOfPlayers newHand newPlayerTurn playedLetters timeout =         
        {
            board = newBoard; 
            dict = newDict;  
            playerNumber = newPlayernumber; 
            numberOfPlayers = newNumberOfPlayers; 
            hand = newHand
            playerTurn = newPlayerTurn
            playedLetters = playedLetters
            timeout  = timeout
        }

    let board st           = st.board
    let dict st            = st.dict
    let playerNumber st    = st.playerNumber
    let numberOfPlayers st = st.numberOfPlayers
    let hand st            = st.hand
    let playerTurn st      = st.playerTurn
    let playedLetters st   = st.playedLetters
    let timeout st         = st.timeout

    // Given a list of moves, insert played letters into the state (playedLetters)
    let insertMovesIntoState (moves:list<coord * (uint32 * (char * int))>) (state:state) =
        List.fold (fun acc move ->
            let (coord, (_,(char, charPoints))) = move
            //debugPrint (sprintf "Inserting move %A %A\n" coord (char))
            let newPlayedLetters = acc.playedLetters |> Map.add coord (char, charPoints)
            mkState acc.board acc.dict acc.playerNumber acc.numberOfPlayers acc.hand acc.playerTurn newPlayedLetters acc.timeout
        ) state moves
    
    let updateHand moves st newPieces =
                // get a multiset of the indexes (uint) of the tiles you played
                let playedIndexes = 
                    moves 
                    |> Seq.map (fun move -> 
                        let (_, (charuint, (_, _))) = move
                        charuint
                        ) 
                    |> Seq.toList 
                    |> MultiSet.ofList

                // remove played tiles from your hand
                let subtractedHand = MultiSet.subtract (hand st) playedIndexes

                // add the new tiles to your hand
                List.fold (fun acc (indexOfLetter, letterCount) -> 
                MultiSet.add indexOfLetter letterCount acc) subtractedHand newPieces


    // Returns list of starters. A starter is a word we can play that starts at the given coord. 
    // The form is (word, (coord, direction))
    let wordLookup playedLetters = 
        let directionalWordLookup playedLetters dir =
            let getExistingWord (coord:coord) (direction:dir) (playedLetters) =                
                let rec recursivelyMoveInDir (coord:coord) (direction:dir) (playedLetters) acc =
                    let (x, y) = coord
                    let (dx, dy) = dir

                    // Check next coord, if there is a letter
                    let newCoord = (x + dx, y + dy)
                    match Map.tryFind newCoord playedLetters with
                    | None -> 
                        // If we hit an empty square, return the word
                        acc
                    | Some (letter, _) -> 
                        // If we hit a letter, recursively move in the same direction
                        recursivelyMoveInDir newCoord direction playedLetters (acc + string letter)
                
                // Check if letter is in middle of word
                let (x, y) = coord
                let (dx, dy) = dir
                let checkPrevious = (x - dx, y - dy)
                match Map.tryFind checkPrevious playedLetters with
                | None ->
                    // If none, then we can move in the direction (because we are at the start of the word)) 
                    let letter = (string (fst(Map.find coord playedLetters)))
                    Some (recursivelyMoveInDir coord direction playedLetters letter)
                | Some _ -> 
                    // If there is a letter, then we are in the middle of a word.
                    None

            // Get the existing word in the given direction.
            Map.fold (fun (acc:List<(string * (coord * dir))>) (coord:coord) ((letter:char), _) -> 
                let existingWord = getExistingWord coord dir playedLetters
                match existingWord with
                | Some word -> 
                    // If we found a word, add it to the list
                    (word, (coord, dir)) :: acc
                | None -> 
                    // If we didn't find a word, return the list
                    acc
                ) [] playedLetters

        // Get the existing words right and down direction.
        let downWords = directionalWordLookup playedLetters (0,1)
        let rightWords = directionalWordLookup playedLetters (1,0)
        
        // Merge the two lists
        downWords@rightWords

module Scrabble =
    open System.Threading
    
    type CoordinatorMessage =
    | AddWord of string * (coord * (int * int))
    | OutOfTime
    | GetLongestWord of AsyncReplyChannel<string * (coord * (int * int))>
                       
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
                    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

                    // Find the longest word we can play on our hand
                    let longestWord = MoveFinder.getLongestWordFirstMove (State.hand st) "" st.dict st.playedLetters (0,0) (1,0)
                    
                    debugPrint(sprintf "**** Playing word: %A ****\n" longestWord )
                    
                    let move = Utility.wordToMove longestWord (0,0) (1, 0) (State.playedLetters st)
                    send cstream (SMPlay move)
                else
                    // Continuing move
                    
                    let mutable listOfWords : (string * (coord * (int * int))) list = List.empty<string * (coord * (int * int))>
                    let mailboxWithList = MailboxProcessor.Start(fun inbox ->
                        let rec messageLoop () = async{
                            let! (msg : CoordinatorMessage) = inbox.Receive()
                            match msg with
                            | AddWord (word,(dir,pos)) ->
                                listOfWords <- (word,(dir,pos))::listOfWords
                            | OutOfTime -> return ()
                            | GetLongestWord replyChannel ->
                                let longestWord =
                                    List.fold (fun (acc:string * (coord * (int * int))) (word:string * (coord * (int * int))) ->
                                        if ((fst word).Length) > ((fst acc).Length) then
                                            word
                                        else
                                            acc
                                    ) ("",((0,0),(0,0))) listOfWords
                                replyChannel.Reply(longestWord)
                            return! messageLoop ()
                        }
                        messageLoop ())
                    
                    let mailboxWithOperations = MailboxProcessor.Start(fun inbox ->
                        let rec messageLoop () = async{
                            let! (msg : list<string * (coord * (int * int))>) = inbox.Receive()
                            
                            msg |> Seq.map (fun word ->
                                async {
                                    // Convert the word to a move
                                    let move = Utility.wordToMove (fst word) (fst (snd word)) (snd (snd word)) (State.playedLetters st)
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
                                    if stateValid then
                                        mailboxWithList.Post ( AddWord (word))
                                        debugPrint (sprintf "I POSTED ASYNC\n")
                                })
                            |> Async.Parallel
                            |> Async.Ignore
                            |> Async.RunSynchronously
                            
                            return! messageLoop ()
                        }
                        messageLoop ())
                    
                    
                    debugPrint("**** Playing move continuing off what is currently on the board ****\n")
                    
                    // Find all words already on teh board
                    let starters = State.wordLookup st.playedLetters

                    let listOfAllWordsWeCanPlay =
                        List.fold (fun (acc:list<string * (coord * (int * int))>) (key,value) ->
                            let (coord, dir) = value
                            let longestWord = MoveFinder.getLongestWordContinuation (State.hand st) key (State.dict st) st.playedLetters coord dir
                            if longestWord.Length > 0 then
                                (longestWord, value)::acc
                            else
                                acc
                        ) [] starters
                        
                    debugPrint(sprintf "**** List of all words we can play: ****\n%A\n" listOfAllWordsWeCanPlay)
                   
                    mailboxWithOperations.Post (listOfAllWordsWeCanPlay)
                                       
                    match st.timeout with
                    | Some time -> Thread.Sleep(int (time - (uint32 100)))
                    | None -> Thread.Sleep(2000)
                    
                    let longestWordWeCanPlay = mailboxWithList.PostAndReply(GetLongestWord)
                    debugPrint (sprintf "Longest word from mailbox: %A\n" (fst longestWordWeCanPlay))
                    
                    debugPrint (sprintf "\n\n======== Longest word we can play =========\n%A\n" (fst longestWordWeCanPlay))
                    
                    let move = Utility.wordToMove (fst longestWordWeCanPlay) (fst (snd longestWordWeCanPlay)) (snd (snd longestWordWeCanPlay)) (State.playedLetters st)
                    send cstream (SMPlay move)
            else
                debugPrint("\n=======================\n**** OPPONENT TURN ****\n=======================\n")
        

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

                // Update hand
                let newHand = State.updateHand moves st newPieces

                // Update the state
                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberOfPlayers st) newHand (State.playerTurn st) updatedStateLetters.playedLetters st.timeout
                
                aux newState false
            | RCM (CMPlayed (pid, moves, points)) ->
                (* Successful play by other player. Update your state *)
                debugPrint (sprintf "============ CMPlayed ============\n")

                // Update playedLetters with new moves
                let updatedStateLetters = State.insertMovesIntoState moves st

                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberOfPlayers st) (State.hand st) (State.playerTurn st) updatedStateLetters.playedLetters st.timeout
                aux newState (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM (CMPassed (pid)) ->
                debugPrint (sprintf "============ OTHER PLAYER PASSED ============\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
                aux st (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM (CMPlayFailed (pid, moves)) ->
                debugPrint (sprintf "============ CMPlayFailed ============\n")
                (* Failed play. Update your state *)
                aux st (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st false

        aux st (st.playerTurn = st.playerNumber)
                
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers handSet playerTurn Map.empty timeout)
        