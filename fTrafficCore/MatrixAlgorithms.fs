namespace fTrafficCore

module Algorithms =
    
    let markingOfConnectedComponents matrix =
        // up to 100 markers
        let parents = Array2D.create 10 2 0

        let rec find x =
            let index = Array.tryFindIndex ((=)x) parents.[1,*]
            match index with
            | Some(i) -> 
                match parents.[i,0] with
                | p when p <> 0 -> find p
                | _ -> x
            | None -> x

        let union x y =
            let j = find x
            let k = find y
            if j <> k then parents.[k,1] <- j

        // returns up and left neighbors of pixel
        let neighbors_labels x y =
            match (x, y) with
            | (0, 0) -> []
            | (0, _) -> [matrix.values.[0, y-1]]
            | (_, 0) -> [matrix.values.[x-1, 0]]
            | _ -> [matrix.values.[x-1, y]; matrix.values.[x, y-1]]

        let mutable label = 0
        let step1 = matrix.values
                    |> Array2D.mapi (fun x y v ->
                                        if v = 1 then
                                            let n = neighbors_labels x y
                                            let m = if n.IsEmpty then
                                                        label <- label + 1
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