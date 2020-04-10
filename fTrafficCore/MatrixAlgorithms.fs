namespace fTrafficCore

module Algorithms =

    let markingOfConnectedComponents matrix =

        // up to 10 markers
        let parents = Array2D.init 2 10 (fun x y -> if x = 0 then y+1 else 0)
        //let parents = array2D [[1;2;3;4;5;6;7;8]
        //                       [2;3;0;3;7;7;0;3]]

        // create a zero initialized copy
        let step1 = (Matrix.cloneO matrix).values

        let rec find x =
            let index = Array.tryFindIndex ((=)x) parents.[0, *]
            match index with
            | Some(i) -> 
                match parents.[1, i] with
                | p when p <> 0 -> find p
                | _ -> x
            | None -> x

        let union x y =
            let j = find x
            let k = find y
            if j <> k then parents.[1, k-1] <- j

        // returns up and left neighbors of pixel
        let neighbors_labels x y =
            match (x, y) with
            | (0, 0) -> []
            | (0, _) -> [step1.[0, y-1]]
            | (_, 0) -> [step1.[x-1, 0]]
            | _ -> [step1.[x, y-1]; step1.[x-1, y]]
            |> List.filter ((<>)0)

        let mutable label = 0
        matrix.values
        |> Array2D.iteri (fun x y v ->
                            if v = 1 then
                                let n = neighbors_labels x y
                                let m = if n.IsEmpty then
                                            label <- label + 1
                                            label
                                        else
                                            n |> List.min
                                n |> List.iter (fun v -> if v <> m then union m v)
                                step1.[x, y] <- m)

        //printfn "%A" parents

        let step2 = matrix.values
                    |> Array2D.mapi (fun x y v ->
                            if v = 1 then step1.[x, y] <- find step1.[x, y]
                            step1.[x, y])

        { values = step2 }

    let recMarkingOfConnectedComponents matrix =

        let copy = Matrix.clone matrix

        let (|Value|Zero|Out|) (x, y) = 
                    if x < 0 || y < 0
                        || x > (copy.values.[0, *].Length - 1)
                        || y > (copy.values.[*, 0].Length - 1) then
                        Out
                    else
                        let row = copy.values.[y, *]
                        match row.[x] with
                            | 0 -> Zero
                            | v -> Value(v)

        let rec markBits x y value =
            match (x, y) with
            | Value(v) ->
                if value > v then
                    copy.values.[y, x] <- value
                    markBits (x + 1) y value
                    markBits (x - 1) y value
                    markBits x (y + 1) value
                    markBits x (y - 1) value
            | Zero | Out -> ()

        let mutable value = 2
        copy.values
        |> Array2D.iteri (fun y x v -> if v = 1 then
                                            markBits x y value
                                            value <- value + 1)

        copy