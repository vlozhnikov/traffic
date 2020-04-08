namespace fTrafficCore

module Algorithms =
    
    let markingOfConnectedComponents matrix =
        // up to 100 markers
        let parents = Array2D.create 10 2 0
        //let parents = array2D [[1;2;3;4;5;6;7;8]
        //                       [2;3;0;3;7;7;0;3]]

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
            if j <> k then parents.[1, k] <- j

        // returns up and left neighbors of pixel
        let neighbors_labels x y =
            match (x, y) with
            | (0, 0) -> []
            | (0, _) ->
                let v = matrix.values.[0, y-1]
                if v = 0 then [parents.[v, 0]]
                else []
            | (_, 0) ->
                let v = matrix.values.[x-1, 0]
                if v <> 0 then [parents.[v, 0]] else []
            | _ ->
                let v1 = matrix.values.[x, y-1]
                let v2 = matrix.values.[x-1, y]

                if v1 = 0 && v2 = 0 then []
                elif v1 <> 0 && v2 <> 0 then [parents.[v1, 0]; parents.[v2, 0]]
                elif v1 <> 0 then [parents.[v1, 0]]
                else [parents.[v2, 0]]

        let mutable label = 0
        let step1 = matrix.values
                    |> Array2D.mapi (fun x y v ->
                                        if v = 1 then
                                            let n = neighbors_labels x y
                                            let m = if n.IsEmpty then
                                                        label <- label + 1
                                                        parents.[0,0] <- label
                                                        label
                                                    else
                                                        n |> List.min
                                            n |> List.iter (fun v -> if v <> m then union m v)
                                            m
                                        else 0)

        printfn "%A" step1
        printfn "%A" parents

        let step2 = matrix.values
                    |> Array2D.mapi (fun x y v ->
                            if v = 1 then step1.[x, y] <- find step1.[x, y]
                            step1.[x, y])

        { values = step2 }