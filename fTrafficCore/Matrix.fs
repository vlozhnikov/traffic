namespace fTrafficCore

module Matrix =

    type Matrix = { values: int[,] }
        with

            static member (==) (matrix1, matrix2) =
                let rowLength = matrix1.values.[0,*].Length
                let colLength = matrix1.values.[*,0].Length

                (rowLength = colLength)

            // sum two matrixs
            static member (+) (matrix1, matrix2) =
                if matrix1 == matrix2 then
                    let array2d = matrix1.values |> Array2D.mapi (fun x y v -> matrix2.values.[x, y] + v)
                    { values = array2d }
                else failwith "matrix1 is not equal to matrix2"

            // multiple matrix and value
            static member (*) (matrix, value) = 
                let array2d = matrix.values |> Array2D.mapi (fun _ _ v -> v * value)
                { values = array2d }

            // creates negative matrix
            member m.negative = 
                m*(-1)

            // substration of matrixs
            static member (-) (matrix1: Matrix, matrix2: Matrix) = 
                if matrix1 == matrix2 then
                    matrix1 + matrix2.negative
                else failwith "matrix1 is not equal to matrix2"

            // returns true when first matrix is matched to second matrix
            static member isMatched matrix1 matrix2 = 
                let cols1 = matrix1.values.[0,*].Length
                let rows2 = matrix2.values.[*,0].Length

                cols1 = rows2

            static member (*) (matrix1, matrix2) =
                if Matrix.isMatched matrix1 matrix2 then
                    ()
                else failwith "matrix1 is not matched to matrix2"
        end

    // init from array2d
    let ofArray2D (values: int [,]) = 
        { values = values }

    // creates zero matrix
    let zero r c =
        let array2d = Array2D.zeroCreate r c
        { values = array2d }

    // creates unit matrix
    let unit r c =
        let array2d = Array2D.init r c (fun x y -> if x = y then 1 else 0)
        { values = array2d }

    // creates triangular matrix
    let triangular matrix =
        let triangular = matrix.values |> Array2D.mapi (fun x y v -> if y < x then 0 else v)
        { values = triangular }

    // creates diagonal matrix
    let diagonal matrix =
        let diagonal = matrix.values |> Array2D.mapi (fun x y v -> if x <> y then 0 else v)
        { values = diagonal }

    // creates square matrix
    let square matrix =
        let rowLength = matrix.values.[0,*].Length
        let colLength = matrix.values.[*,0].Length
        let length = System.Math.Min (rowLength, colLength)

        let zero = Array2D.zeroCreate length length
        let square = zero |> Array2D.mapi (fun x y _ ->
                                              let i = y*length+x
                                              matrix.values.[x, y]
                                          )
        { values = square }