namespace fTrafficCore

[<Struct>]
type Matrix = { values: int[,] }
    with

        /// <summary>Init matrix by values</summary>
        /// <param name="values">values</param>
        /// <returns>Matrix</returns>
        static member ofArray2D (values: int [,]) = 
            { values = values }

        /// <summary>Init matrix by one-dimension list values</summary>
        /// <param = "matrix">Matrix</param>
        /// <param = "rows">Count of rows</param>
        /// <param = "cols">Count of cold</param>
        /// <returns>Matrix</returns>
        static member ofArray rows cols (values: int list) = 
            let matrix = Matrix.O rows cols
            let values = matrix.values
                         |> Array2D.mapi (fun x y v ->
                                              let index = x * cols + y
                                              values.[index]
                                         )

            { values = values }

        /// <summary>Clone matrix from another matrix</summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>Matrix</returns>
        static member clone matrix =
            let values = Array2D.copy matrix.values
            { values = values }

        /// <summary>Clone empty matrix from another matrix</summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>Empty matrix</returns>
        static member cloneO matrix =
            let (rows, cols) = Matrix.sizes matrix
            Matrix.O rows cols

        /// <summary>Create empty matrix</summary>
        /// <param name="rows">Count of rows</param>
        /// <param name="cols">Count of columns</param>
        /// <returns>Matrix</returns>
        static member O rows cols =
            let values = Array2D.zeroCreate rows cols
            { values = values }

        /// <summary>Create unit matrix (main diagonal filled with 1)</summary>
        /// <param name="rows">Count of rows</param>
        /// <param name="cols">Count of columns</param>
        /// <returns>Unit matrix</returns>
        static member E rows cols =
            let values = Array2D.init rows cols (fun x y -> if x = y then 1 else 0)
            { values = values }

        /// <summary>
        /// Create triangular matrix (cells under main diagonal filled with 0) from another matrix
        /// </summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>Triangular matrix</returns>
        static member T matrix =
            let values = matrix.values |> Array2D.mapi (fun x y v -> if y < x then 0 else v)
            { values = values }

        /// <summary>
        /// Create diagonal matrix (cells out from main diagonal filled with 0) from another matrix
        /// </summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>Diagonal matrix</returns>
        static member D matrix =
            let values = matrix.values |> Array2D.mapi (fun x y v -> if x <> y then 0 else v)
            { values = values }

        /// <summary>Cut square matrix from another matrix</summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>Square matrix</returns>
        static member toSquare matrix =

            let (rows, cols) = Matrix.sizes matrix

            let length = System.Math.Min (rows, cols)
            let zero = Array2D.zeroCreate length length

            let values = zero |> Array2D.mapi (fun x y _ -> matrix.values.[x, y])
            { values = values }

        /// <summary>Transpose the matrix</summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>Transpose matrix</returns>
        static member transpose matrix =
            let (rows, cols) = Matrix.sizes matrix

            let tMatrix = Matrix.O cols rows
            matrix.values |> Array2D.iteri(fun x y v -> tMatrix.values.[y, x] <- v)

            tMatrix

        /// <summary>Transpose the matrix and fill it by zeros</summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>Matrix</returns>
        static member transposeO matrix =
            let (rows, cols) = Matrix.sizes matrix
            let tMatrix = Matrix.O cols rows

            tMatrix

        /// <summary>Get size of matrix</summary>
        /// <param name="matrix">Origin matrix</param>
        /// <returns>tuple which contains the number of rows and columns</returns>
        static member sizes matrix =
            let rows = matrix.values.[*,0].Length
            let cols = matrix.values.[0,*].Length

            (rows, cols)

        /// <summary>Compare dimensions of matrices</summary>
        /// <param name="matrix1">First matrix</param>
        /// <param name="matrix2">Second matrix</param>
        /// <returns>true if matrices have same sizes</returns>
        static member isEquallySized matrix1 matrix2 =
            let dim1 = Matrix.sizes matrix1
            let dim2 = Matrix.sizes matrix2

            (dim1 = dim2)

        /// <summary>Compare matrices</summary>
        /// <param name="matrix1">First matrix</param>
        /// <param name="matrix2">Second matrix</param>
        /// <returns>true if matrices are equal</returns>
        static member (==) (matrix1, matrix2) =

            if not (Matrix.isEquallySized matrix1 matrix2) then false
            else
                not (matrix1.values
                     |> Array2D.mapi (fun x y v -> if matrix2.values.[x, y] <> v then false else true)
                     |> Seq.cast<bool>
                     |> Seq.contains false)

        /// <summary>Sum of matrices</summary>
        /// <param name="matrix1">First matrix</param>
        /// <param name="matrix2">Second matrix</param>
        /// <returns>Matrix</returns>
        /// <exception cref="failwith">Thrown when matrix1 is not equal to matrix2</exception>
        static member (+) (matrix1, matrix2) =
            if Matrix.isEquallySized matrix1 matrix2 then
                let values = matrix1.values |> Array2D.mapi (fun x y v -> matrix2.values.[x, y] + v)
                { values = values }
            else failwith "matrix1 is not equal to matrix2"

        /// <summary>Multiply value and matrix</summary>
        /// <param name="value">Value</param>
        /// <param name="matrix">Matrix</param>
        /// <returns>Matrix</returns>
        static member (*) (value, matrix) = 
            let values = matrix.values |> Array2D.mapi (fun _ _ v -> v * value)
            { values = values }

        /// <summary>Sum matrix and value</summary>
        /// <param name="matrix">Matrix</param>
        /// <param name="value">Value</param>
        /// <returns>Matrix</returns>
        static member (+) (matrix, (value: int)) =
            let (rows, cols) = Matrix.sizes matrix
            let unit = Matrix.E rows cols

            value * unit + matrix

        /// <summary>Subtract the mitrices</summary>
        /// <param name="matrix1">First matrix</param>
        /// <param name="matrix2">Second matrix</param>
        /// <returns>Matrix</returns>
        /// <exception cref="failwith">Thrown when matrix1 is not equal to matrix2</exception>
        static member (-) (matrix1: Matrix, matrix2: Matrix) = 
            if Matrix.isEquallySized matrix1 matrix2 then
                matrix1 + (-1) * matrix2
            else failwith "matrix1 is not equal to matrix2"

        /// <summary>
        /// Matrix matching (number of rows of rirst matrix should be same number of columns of second matrix)
        /// </summary>
        /// <param name="matrix1">First matrix</param>
        /// <param name="matrix2">Second matrix</param>
        /// <returns>true if matrix1 matches the matrix2</returns>
        static member isMatched matrix1 matrix2 = 
            let row1 = matrix1.values.[0,*].Length
            let col2 = matrix2.values.[*,0].Length

            row1 = col2

        /// <summary>Matrix multiplication</summary>
        /// <param name="matrix1">First matrix</param>
        /// <param name="matrix2">Second matrix</param>
        /// <returns>Matrix</returns>
        /// <exception cref="failwith">Thrown when matrix1 is not matched to matrix2</exception>
        static member (*) (matrix1, (matrix2: Matrix)) =
            if Matrix.isMatched matrix1 matrix2 then
                let row1 = matrix1.values.[*,0].Length
                let col2 = matrix2.values.[0,*].Length

                let values = Array2D.zeroCreate row1 col2

                for r in 0..row1-1 do
                    for c in 0..col2-1 do
                        let row = Array.toList matrix1.values.[r,*]
                        let col = Array.toList matrix2.values.[*,c]

                        let cell = List.fold2 (fun acc val1 val2 -> acc + (val1 * val2)) 0 row col
                        values.[r,c] <- cell

                { values = values }

            else failwith "matrix1 is not matched to matrix2"

        /// <summary>Expomantiation matrix</summary>
        /// <param name="matrix">Matrix</param>
        /// <param name="value">Value degree</param>
        /// <returns>Matrix</returns>
        static member (^^) (matrix, value) =

            let inRecPow m p =

                let rec recPow acc p =
                    match p with
                    | x when x > 0 ->
                        let nextAcc = acc*m
                        recPow nextAcc (x-1)
                    | _ -> acc

                let (rows, cols) = Matrix.sizes matrix
                let u = Matrix.E rows cols

                recPow u p

            let powMatrix = inRecPow matrix value
            { values = powMatrix.values }

        /// <summary>Clockwise rotation of matrix by 90 degrees</summary>
        /// <param name="matrix">Matrix</param>
        /// <returns>Matrix</returns>
        static member rotate90 matrix =

            let (rows, cols) = Matrix.sizes matrix
            let transpose = Matrix.transposeO matrix

            for r in [0..rows-1] do
                let row = matrix.values.[r,*]
                transpose.values.[*,cols-r] <- row

            transpose

        /// <summary>Clockwise rotation of matrix by 270 degrees</summary>
        /// <param name="matrix">Matrix</param>
        /// <returns>Matrix</returns>
        static member rotate270 matrix =

            let (rows, cols) = Matrix.sizes matrix
            let transpose = Matrix.transposeO matrix

            for r in [0..rows-1] do
                let row = Array.rev matrix.values.[r,*]
                transpose.values.[*,r] <- row

            transpose

        /// <summary>Clockwise rotation of matrix by 180 degrees</summary>
        /// <param name="matrix">Matrix</param>
        /// <returns>Matrix</returns>
        static member rotate180 matrix =
            let (_, cols) = Matrix.sizes matrix
            let rMatrix = Matrix.cloneO matrix

            for r in [0..cols-1] do
                let col = Array.rev matrix.values.[*,r]
                rMatrix.values.[*,r] <- col

            rMatrix

        /// <summary>
        /// Calculates 2x matrix determinant
        /// </summary>
        /// <param name="matrix">Matrix</param>
        /// <returns>Determinant value</returns>
        static member determinant2x matrix =
            let (rows, cols) = Matrix.sizes matrix
            if rows = 2 && cols = 2 then
                let values = matrix.values
                values.[0,0]*values.[1,1]-values.[0,1]*values.[1,0]
            else failwith "Matrix is not 2x"

        /// <summary>
        /// Calculates 3x matrix determinant
        /// </summary>
        /// <param name="matrix">Matrix</param>
        /// <returns>Determinant value</returns>
        static member determinant3x matrix =
            let (rows, cols) = Matrix.sizes matrix
            if rows = 3 && cols = 3 then
                let values = matrix.values
                values.[0,0]*values.[1,1]*values.[2,2]+
                values.[2,0]*values.[0,1]*values.[1,2]+
                values.[1,0]*values.[2,1]*values.[0,2]-
                values.[0,2]*values.[1,1]*values.[2,0]-
                values.[1,0]*values.[0,1]*values.[2,2]-
                values.[2,1]*values.[1,2]*values.[0,0]
            else failwith "Matrix is not 3x"

        /// <summary>
        /// Calculates matrix determinant
        /// </summary>
        /// <param name="matrix">Matrix</param>
        /// <returns>Determinant value</returns>
        static member determinant matrix = 
            let (rows, cols) = Matrix.sizes matrix

            if rows <> cols then failwith "the matrix should be square"
            else

                // cut row and col from matrix
                let cutRowCol m r c = 
                    let d = Matrix.sizes m

                    let matrix = m.values
                                 |> Array2D.mapi (fun x y v -> if x <> r && y <> c then Some(v) else None)
                                 |> Seq.cast<int option>
                                 |> Seq.filter (fun v -> v.IsSome)
                                 |> Seq.map (fun v -> v.Value)
                                 |> List.ofSeq
                                 |> Matrix.ofArray (fst d - 1) (snd d - 1)
                    matrix

                let rec recDet m = 
                    let d = Matrix.sizes m
                    match (fst d, snd d) with
                    | 3, 3 -> (Matrix.determinant3x m)
                    | 2, 2 -> (Matrix.determinant2x m)
                    | x, y when x > 3 && y > 3 ->
                        m.values.[0,*] // caclulate the matrix determinant by first row
                        |> Array.mapi (fun index elem ->
                            if elem = 0 then 0
                            else
                                let minorMatrix = cutRowCol m 0 index
                                elem*(if ((0 + index) % 2) = 0 then 1 else -1)*(recDet minorMatrix)
                            )
                        |> Array.sum
                    | _, _ -> m.values.[0,0]

                let det = recDet matrix
                det

        static member subMatrix row col width height matrix =
            let (rows, cols) = Matrix.sizes matrix
            if rows < (row + height) || cols < (col + width) then failwith "submatrix is out of range"
            let values = matrix.values.[row..(row+height-1), col..(col+width-1)]
            {values = values}

    end