namespace fTrafficCore

module Matrix =

    type Matrix = { values: int[,] }
        with

            // init from array2d
            static member ofArray2D (values: int [,]) = 
                { values = values }

            // creates zero matrix
            static member O r c =
                let array2d = Array2D.zeroCreate r c
                { values = array2d }

            // creates unit matrix
            static member E r c =
                let array2d = Array2D.init r c (fun x y -> if x = y then 1 else 0)
                { values = array2d }

            // creates triangular matrix
            static member triangular matrix =
                let triangular = matrix.values |> Array2D.mapi (fun x y v -> if y < x then 0 else v)
                { values = triangular }

            // creates diagonal matrix
            static member D matrix =
                let diagonal = matrix.values |> Array2D.mapi (fun x y v -> if x <> y then 0 else v)
                { values = diagonal }

            // creates square matrix
            static member square matrix =

                let dim = Matrix.sizes matrix
                let colCount: int = snd dim
                let rowCount: int = fst dim
                let length = System.Math.Min (colCount, rowCount)

                let zero = Array2D.zeroCreate length length
                let square = zero |> Array2D.mapi (fun x y _ ->
                                                      let i = y*length+x
                                                      matrix.values.[x, y]
                                                  )
                { values = square }

            // transpose of matrix
            static member T matrix =
                let dim = Matrix.sizes matrix
                let rows = fst dim
                let cols = snd dim

                let tMatrix = Matrix.O cols rows
                matrix.values |> Array2D.iteri(fun x y v -> tMatrix.values.[y, x] <- v)

                tMatrix

            // returns sizes of matrix
            static member sizes matrix =
                let rowCount = matrix.values.[*,0].Length
                let colCount = matrix.values.[0,*].Length
                (rowCount, colCount)

            // true if sizes are same
            static member isEquallySized matrix1 matrix2 =

                let dim1 = Matrix.sizes matrix1
                let dim2 = Matrix.sizes matrix2

                (dim1 = dim2)

            // true if all elements are same
            static member (==) (matrix1, matrix2) =

                if not (Matrix.isEquallySized matrix1 matrix2) then false
                else
                    matrix1.values
                        |> Array2D.mapi (fun x y v -> if matrix2.values.[x, y] <> v then false else true)
                        |> Seq.cast<bool>
                        |> Seq.fold (fun acc elem -> acc && elem) true

            // sum two matrixs
            static member (+) (matrix1, matrix2) =
                if Matrix.isEquallySized matrix1 matrix2 then
                    let array2d = matrix1.values |> Array2D.mapi (fun x y v -> matrix2.values.[x, y] + v)
                    { values = array2d }
                else failwith "matrix1 is not equal to matrix2"
                
            // multiple matrix and value
            static member (*) (value, matrix) = 
                let array2d = matrix.values |> Array2D.mapi (fun _ _ v -> v * value)
                { values = array2d }

            // sum matrix and value
            static member (+) (matrix, (value: int)) =
                let dim = Matrix.sizes matrix
                let r = fst dim
                let c = snd dim

                let unit = Array2D.init r c (fun x y -> if x = y then 1 else 0)
                let unitMatrix = { values = unit }

                value*unitMatrix + matrix

            // substration of matrixs
            static member (-) (matrix1: Matrix, matrix2: Matrix) = 
                if Matrix.isEquallySized matrix1 matrix2 then
                    matrix1 + (-1)*matrix2
                else failwith "matrix1 is not equal to matrix2"

            // returns true when first matrix is matched to second matrix
            static member isMatched matrix1 matrix2 = 
                let row1Count = matrix1.values.[0,*].Length
                let col2Count = matrix2.values.[*,0].Length

                row1Count = col2Count

            // multiplication of matrixs
            static member (*) (matrix1, (matrix2: Matrix)) =
                if Matrix.isMatched matrix1 matrix2 then
                    let row1Count = matrix1.values.[*,0].Length
                    let col2Count = matrix2.values.[0,*].Length

                    let values = Array2D.zeroCreate row1Count col2Count

                    for r in 0..row1Count-1 do
                        for c in 0..col2Count-1 do
                            let row = Array.toList matrix1.values.[r,*]
                            let col = Array.toList matrix2.values.[*,c]

                            let cell = List.fold2 (fun acc val1 val2 -> acc + (val1 * val2)) 0 row col
                            values.[r,c] <- cell

                    { values = values }

                else failwith "matrix1 is not matched to matrix2"

            // pow of matrixs
            static member (^^) (matrix, value) =

                let inRecPow m p =

                    let rec recPow acc p =
                        match p with
                        | x when x > 0 ->
                            let nextAcc = acc*m
                            recPow nextAcc (x-1)
                        | _ -> acc

                    let dim = Matrix.sizes matrix
                    let colCount = snd dim
                    let rowCount = fst dim

                    let u = Matrix.E rowCount colCount
                    recPow u p

                let powMatrix = inRecPow matrix value
                { values = powMatrix.values }
        end