// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

  type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32> 

  // Green exercises (multiset)
  let empty = MS (Map.empty)
  let isEmpty (MS(s)) = Map.isEmpty s 
  let size (MS(s)) = Map.fold (fun acc _ value -> acc + value) 0u s
  let contains key (MS(s)) = Map.containsKey key s
  let numItems key (MS(s)) = Map.tryFind key s |> Option.defaultValue (uint32 0)
  let add key n (MS(s)) = MS(s.Add (key, ((numItems key (MS(s))) + n)))
  let addSingle key (MS(s)) = add key 1u (MS(s))
  let remove key n (MS(s)) = 
    let occurences = numItems key (MS(s))
    if (occurences > n) then MS(s.Remove(key).Add(key, (occurences - n)))
    else MS(s.Remove key)
  let removeSingle key (MS(s)) = remove key 1u (MS(s))
  let fold f acc (MS(s)) = Map.fold f acc s
  let foldBack f (MS(s)) acc = Map.foldBack f s acc

  // Yellow exercises (multiset)
  let ofList lst = List.fold (fun acc elem -> addSingle elem acc) empty lst
  let toList s = foldBack (fun elem num acc -> List.init (int32 num) (fun _ -> elem) @ acc) s []
  let map f s = ofList (List.map f (toList s))
  let union s1 s2 = fold (fun acc elem s1current ->
            let s2current = (numItems elem acc)
            if (s2current < s1current) then
                remove elem s2current acc |> add elem s1current
            else empty |> add elem s2current) s2 s1

  let sum s1 s2 = fold (fun acc elem s1current -> 
            let s2current = (numItems elem s2) 
            add elem (s1current + s2current) acc) empty s1
    
  let subtract s1 s2 =
      fold (fun acc elem _ ->
          remove elem (numItems elem s2) acc) s1 s2

  let intersection s1 s2 =
    fold (fun acc elem s1current ->
        let s2current = (numItems elem s2)
        if s1current = s2current then
            add elem s1current acc
        else
            empty   
        ) empty s1
