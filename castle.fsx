(* find route (calculating is better than scheming) *)

//Copyright (c) 2016 by André van Meulebrouck.  All rights reserved worldwide.

let uniqueDoors = 
    List.fold (fun acc pair -> acc |> Set.add pair) (Set.empty : Set<string * string>)

let uniqueRooms = 
    Set.fold (fun acc (x, y) -> acc |> Set.add x |> Set.add y) Set.empty

let adjacent pairs target =
    let fct acc (x, y) =
        if x = target then 
            y :: acc
        elif y = target then
            x :: acc
        else acc
    List.fold fct [] pairs

let adjacencies pairs =
    let doors = uniqueDoors pairs
    let rooms = uniqueRooms doors
    let findAdj = adjacent <| Set.toList doors
    Set.map (fun x -> (x, findAdj x)) rooms |> Set.toList

let castle = 
    [("a", "x")
    ;("a", "c")
    ;("a", "d")
    ;("a", "o")
    ;("a", "h")
    ;("m", "o")
    ;("x", "o")
    ;("x", "b")
    ;("b", "c")
    ;("d", "e")
    ;("e", "f")
    ;("f", "g")
    ;("k", "l")
    ;("j", "i")
    ;("i", "h")
    ;("i", "n")
    ;("n", "l")
    ;("n", "h")
    ;("h", "d")
    ;("m", "h")
    ;("k", "j")
    ;("m", "phone");]
    |> adjacencies

//   _________________________________________
//   |   _|_                  |      _|_     |
//   |   ___     M            | L    ___  K  |
//   |    |                   |       |      |
//   |    |____| |_____       |__| |__|__| |_|
//   |    |           |       |       |      |
//   |    |   PHONE   |       |  N    |   J  |
//   |    |___________|__| |__| |_| |_|_     |
//   |    |          _|_        _|_    _|_   |
//   | O  |          ___   H    ___ I  ___   |
//   |    |           |___| |____|______|____|
//   |   _|_    A    _|_       _|_     _|_   |
//   |   ___         ___   D   ___  E  ___   |
//   |    |           |_________|_______|    |
//   |    |          _|_              |   F  |
//   |    |          ___     C        |      |
//   |    |____| |____|________| |____|__| |_|
//   |    |          _|_              |      |
//   |   _|_    X    ___       B      |  G   |
//   |   ___          |               |      |
//   |____|___________|_______________|______|

let candidates lst target = 
    let res = List.tryFind (fun (x, y) -> x = target) lst
    match res with
    | None -> None
    | Some (x, y) -> Some y

let route structure start finish = 
    if start = finish then 
        Some [start]
    else
        let getCandidates = candidates structure 
        let rec down current route =
            let options = getCandidates current
            match options with
            | None -> None
            | Some x -> 
                if List.contains finish x then 
                    finish :: current :: route |> List.rev |> Some
                else 
                    let rec across doors route =
                        match doors with
                        | [] -> None
                        | h :: t ->
                            if List.contains h route then 
                                across t route
                            else
                                let intermediate = down h (current :: route)
                                if intermediate.IsSome then intermediate
                                else across t route
                    across x route
        down start []

let rt = route castle

let rt1 = rt "a" "phone"
let rt2 = rt "b" "phone"
let rt3 = rt "g" "phone"
let rt4 = rt "l" "phone"
let rt5 = rt "a" "a"
let rt6 = rt "a" "laundry"
let rt7 = rt "library" "phone"
let rt8 = rt "bedroom" "laundry"

//findRoutes

let routes structure start finish = 
    if start = finish then 
        Some [[start]]
    else
        let rec across candidates route next found =
            match candidates with
            | [] -> next, found
            | h :: t -> 
                if h = finish then
                    across t route next ((h :: route) :: found)
                elif List.contains h route then
                    across t route next found
                else
                    across t route ((h :: route) :: next) found
        let getCandidates = candidates structure 
        let rec down current next found =
            match current with
            | [] -> 
                if List.isEmpty next then
                    if List.isEmpty found then None
                    else found |> List.map List.rev |> List.sortBy List.length |> Some
                else down next [] found
            | ((h :: t) as x) :: r ->
                let candidates = getCandidates h
                match candidates with
                | None -> 
                    down r next found
                | Some y -> 
                    let next, found = across y x next found
                    down r next found
            | _ -> None
        down [[start]] [] []

let rts = routes castle
let rts1 = rts "a" "phone"
let rts2 = rts "a" "g"

//let rt9 = routes castle "a" "phone"

//findOptimalRoute

(* find route *)