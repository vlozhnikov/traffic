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

    /// <summary>
    /// Returns x1, x2, y1, y2 submatrix coordinates according with mask
    /// </summary>
    /// <param name="source">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(x, y)">array2d current coordinate</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>Subarray</returns>
    let subArrayOfMaks source (x, y) (maskCenterX, maskCenterY) = 
        let (rows, cols) = Matrix.sizes {values = source}

        let x1 = if (x - maskCenterX) < 0 then 0 else (x - maskCenterX)
        let y1 = if (y - maskCenterY) < 0 then 0 else (y - maskCenterY)
        let x2 = if (x + maskCenterX) >= cols then (cols - 1) else (x + maskCenterX)
        let y2 = if (y + maskCenterY) >= rows then (rows - 1) else (y + maskCenterY)

        (x1, x2, y1, y2)

    /// <summary>
    /// Upbuilding original array2d algorithm
    /// </summary>
    /// <param name="array2d">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>New array2d</returns>
    let upbuilding array2d mask (centerX, centerY) =

        let sub source mask (x, y) (maskCenterX, maskCenterY) =
            let (x1, x2, y1, y2) = subArrayOfMaks source (x, y) (maskCenterX, maskCenterY)
            source.[x1..x2, y1..y2] <- mask
            

        let copy = (Matrix.cloneO {values = array2d}).values
        array2d |> Array2D.iteri (fun x y v -> if v = 1 then sub copy mask (x, y) (centerX, centerY))

        copy

    /// <summary>
    /// Erosion original array2d algorithm
    /// </summary>
    /// <param name="array2d">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>New array2d</returns>
    let erosion array2d mask (centerX, centerY) =

        let sub source (copy: int [,]) mask (x, y) (maskCenterX, maskCenterY) =

            let (x1, x2, y1, y2) = subArrayOfMaks source (x, y) (maskCenterX, maskCenterY)
            let subArray2d = source.[x1..x2, y1..y2]

            if (Matrix.ofArray2D subArray2d) == (Matrix.ofArray2D mask) then
                copy.[x, y] <- 1

        let copy = (Matrix.cloneO {values = array2d}).values
        array2d |> Array2D.iteri (fun x y v -> if v = 1 then sub array2d copy mask (x, y) (centerX, centerY))

        copy

    /// <summary>
    /// Closure original array2d algorithm
    /// </summary>
    /// <param name="array2d">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>New array2d</returns>
    let closure array2d mask (centerX, centerY) = 
        let step1 = upbuilding array2d mask (centerX, centerY)
        let step2 = erosion step1 mask (centerX, centerY)

        step2

    /// <summary>
    /// Orening original array2d algorithm
    /// </summary>
    /// <param name="array2d">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>New array2d</returns>
    let opening array2d mask (centerX, centerY) = 
        let step1 = erosion array2d mask (centerX, centerY)
        let step2 = upbuilding step1 mask (centerX, centerY)

        step2