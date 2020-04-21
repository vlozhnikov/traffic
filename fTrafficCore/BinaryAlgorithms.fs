namespace fTrafficCore

module Algorithms =

    let markingOfConnectedComponents array2d =

        // up to 10 markers
        let parents = Array2D.init 2 10 (fun x y -> if x = 0 then y+1 else 0)
        //let parents = array2D [[1;2;3;4;5;6;7;8]
        //                       [2;3;0;3;7;7;0;3]]

        // create a zero initialized copy
        let step1 = (Matrix.cloneO (Matrix.ofArray2D array2d)).values

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
        array2d
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

        let step2 = array2d
                    |> Array2D.mapi (fun x y v ->
                            if v = 1 then step1.[x, y] <- find step1.[x, y]
                            step1.[x, y])

        step2

    let recMarkingOfConnectedComponents array2d =

        let copy = (Matrix.cloneO (Matrix.ofArray2D array2d)).values

        let (|Value|Zero|Out|) (x, y) = 
                    if x < 0 || y < 0
                        || x > (copy.[0, *].Length - 1)
                        || y > (copy.[*, 0].Length - 1) then
                        Out
                    else
                        let row = copy.[y, *]
                        match row.[x] with
                            | 0 -> Zero
                            | v -> Value(v)

        let rec markBits x y value =
            match (x, y) with
            | Value(v) ->
                if value > v then
                    copy.[y, x] <- value
                    markBits (x + 1) y value
                    markBits (x - 1) y value
                    markBits x (y + 1) value
                    markBits x (y - 1) value
            | Zero | Out -> ()

        let mutable value = 2
        copy
        |> Array2D.iteri (fun y x v -> if v = 1 then
                                            markBits x y value
                                            value <- value + 1)

        copy

    /// <summary>
    /// Operator >.
    /// </summary>
    /// <param name="value">value</param>
    /// <param name="checkValue">check value</param>
    /// <returns>Value or checkvalue</returns>
    let (>.) value checkValue = if value > checkValue then checkValue else value

    /// <summary>
    /// Operator <.
    /// </summary>
    /// <param name="value">value</param>
    /// <param name="checkValue">check value</param>
    /// <returns>Value or checkvalue</returns>
    let (<.) value checkValue = if value < checkValue then checkValue else value

    let contains array2d mask =
        if Matrix.isEquallySized (Matrix.ofArray2D array2d) (Matrix.ofArray2D mask) then
            not (array2d
                 |> Array2D.mapi (fun x y v -> if mask.[x, y ] = 0 then 1
                                               elif mask.[x, y] = v then 1
                                               else 0)
                 |> Seq.cast<int>
                 |> Seq.contains 0)
        else false

    /// <summary>
    /// Returns x1, x2, y1, y2 submatrix coordinates according with mask
    /// </summary>
    /// <param name="source">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(x, y)">array2d current coordinate</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>Subarray</returns>
    let subArrayOfMaks array2d mask (centerX, centerY) (maskCenterX, maskCenterY) = 
        let (rows, cols) = Matrix.sizes {values = array2d}
        let (maskRows, maskCols) = Matrix.sizes {values = mask}

        let x1 = centerX - maskCenterX
        let y1 = centerY - maskCenterY
        let x2 = x1 + maskCols - 1
        let y2 = y1 + maskRows - 1

        (x1 <. 0, x2 >. (cols - 1), y1 <. 0, y2 >. (rows - 1))

    /// <summary>
    /// Upbuilding original array2d algorithm
    /// </summary>
    /// <param name="array2d">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>New array2d</returns>
    let upbuilding array2d mask (maskCenterX, maskCenterY) =

        let sub source mask (x, y) (maskCenterX, maskCenterY) =
            let (x1, x2, y1, y2) = subArrayOfMaks source mask (x, y) (maskCenterX, maskCenterY)
            source.[y1..y2, x1..x2] <- mask

        let copy = (Matrix.cloneO {values = array2d}).values
        array2d |> Array2D.iteri (fun y x v -> if v = 1 then sub copy mask (x, y) (maskCenterX, maskCenterY))

        copy

    /// <summary>
    /// Erosion original array2d algorithm
    /// </summary>
    /// <param name="array2d">original array2d</param>
    /// <param name="mask">mask</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>New array2d</returns>
    let erosion array2d mask (maskCenterX, maskCenterY) =

        let sub source (dest: int [,]) mask (x, y) (maskCenterX, maskCenterY) =
            let (x1, x2, y1, y2) = subArrayOfMaks source mask (x, y) (maskCenterX, maskCenterY)
            let subArray2d = source.[y1..y2, x1..x2]

            if contains subArray2d mask then
                dest.[y, x] <- 1

        let copy = (Matrix.cloneO {values = array2d}).values
        array2d |> Array2D.iteri (fun y x v -> if v = 1 then sub array2d copy mask (x, y) (maskCenterX, maskCenterY))

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

    //https://habr.com/ru/post/113626/

    /// <summary>
    /// Inverse original array2d algorithm
    /// </summary>
    /// <param name="array2d">original array2d</param>
    /// <returns>Enversed array2d</returns>
    let inverse array2d =
        array2d |> Array2D.mapi (fun _ _ v -> v ^^^ 1)

    /// <summary>
    /// Union two arrays2d algorithm
    /// </summary>
    /// <param name="array2d1">array2d</param>
    /// <param name="array2d2">array2d</param>
    /// <returns>Union</returns>
    let union array2d1 array2d2 = 
        if Matrix.isEquallyArraySized array2d1 array2d2 then
            array2d1 |> Array2D.mapi (fun x y v -> v ||| array2d2.[x, y])
        else failwith "array2d1 is not equal to array2d2"

    /// <summary>
    /// Intersection two arrays2d algorithm
    /// </summary>
    /// <param name="array2d1">array2d</param>
    /// <param name="array2d2">array2d</param>
    /// <returns>Intersection</returns>
    let intersection array2d1 array2d2 = 
        if Matrix.isEquallyArraySized array2d1 array2d2 then
            array2d1 |> Array2D.mapi (fun x y v -> v &&& array2d2.[x, y])
        else failwith "array2d1 is not equal to array2d2"

    /// <summary>
    /// Complement algorithm
    /// </summary>
    /// <param name="array2d">array2d</param>
    /// <returns>Complement</returns>
    let complement array2d = 
        inverse array2d

    /// <summary>
    /// Difference two arrays2d algorithm
    /// </summary>
    /// <param name="array2d1">array2d</param>
    /// <param name="array2d2">array2d</param>
    /// <returns>Difference</returns>
    let difference array2d1 array2d2 = 
        if Matrix.isEquallyArraySized array2d1 array2d2 then
            array2d1 |> Array2D.mapi (fun x y v -> if v <> array2d2.[x, y] then v else 0)
        else failwith "array2d1 is not equal to array2d2"

    /// <summary>
    /// Border allocation algorithm
    /// </summary>
    /// <param name="array2d1">array2d</param>
    /// <param name="array2d2">array2d</param>
    /// <param name="(maskCenterX, maskCenterY)">mask center coordinate</param>
    /// <returns>Border</returns>
    let borderAllocation array2d mask (maskCenterX, maskCenterY) = 
        let e = erosion array2d mask (maskCenterX, maskCenterY)
        let border = difference array2d e
        border