module TrafficCoreTests

open NUnit.Framework
open fTrafficCore.Matrix

[<SetUp>]
let Setup() = ()

[<Test>]
let Test1() = Assert.Pass()

[<Test>]
let MatrixTest() =

    printfn ""

    //--------------------
    let a1 = array2D [[1;0;2]
                      [3;1;0]]
    let A1 = Matrix.ofArray2D a1

    let b1 = array2D [[-1;0]
                      [5;1]
                      [-2;0]]
    let B1 = Matrix.ofArray2D b1

    let R1 = A1*B1

    printfn "A1*B1 =\n %A" R1.values

    //--------------------
    let a2 = array2D [[1;2]
                      [3;4]]
    let A2 = Matrix.ofArray2D a2

    let b2 = array2D [[0;5]
                      [6;8]]
    let B2 = Matrix.ofArray2D b2

    let R21 = A2*B2
    printfn "A2*B2 =\n %A" R21.values

    let R22 = B2*A2
    printfn "B2*A2 =\n %A" R22.values

    //--------------------
    let a3 = array2D [[1;-1]
                      [1;-1]]
    let A3 = Matrix.ofArray2D a3

    let b3 = array2D [[1;1]
                      [1;1]]
    let B3 = Matrix.ofArray2D b3

    let R3 = A3*B3
    printfn "A3*B3 =\n %A" R3.values

    //--------------------
    let a4 = array2D [[1;0]
                      [2;-1]]
    let A4 = Matrix.ofArray2D a4

    let R41 = A4^^2
    printfn "A4^2 =\n %A" R41.values

    let R42 = A4^^3
    printfn "A4^3 =\n %A" R42.values

    let R43 = A4*(R41^^2)
    printfn "A4*A4^2 =\n %A" R43.values

    //--------------------
    let a5 = array2D [[1;0]
                      [2;-1]]
    let A5 = Matrix.ofArray2D a5

    let R5 = 2*(A5^^3) - 4*(A5^^2) + 3
    printfn "2*A5^3 - 4*A5^2 + 3 =\n %A" R5.values

    //--------------------
    let a6 = array2D [[1;0;2]
                      [3;1;0]]
    let A6 = Matrix.ofArray2D a6

    let R6 = Matrix.T A6
    printfn "A6->T = \n %A" R6.values

[<Test>]
let MatrixTest2() =

    let A1 = Matrix.O 5 5
    printfn "//--------------------"
    printfn "O =\n %A" A1.values

    //--------------------
    let A2 = Matrix.E 5 5
    printfn "//--------------------"
    printfn "E = \n %A" A2.values

    //--------------------
    let a3 = array2D [[1;2;3]
                      [4;5;6]
                      [7;8;9]
                      [10;11;12]]
    let A3 = Matrix.ofArray2D a3
    let R3 = Matrix.triangular A3
    printfn "//--------------------"
    printfn "origin = \n %A" A3.values
    printfn "triangular = \n %A" R3.values

    //--------------------
    let a4 = array2D [[1;2;3]
                      [4;5;6]
                      [7;8;9]
                      [10;11;12]]
    let A4 = Matrix.ofArray2D a4
    let R4 = Matrix.D A4
    printfn "//--------------------"
    printfn "origin = \n %A" A4.values
    printfn "diagonal = \n %A" R4.values

    //--------------------
    let a5 = array2D [[1;2;3]
                      [4;5;6]
                      [7;8;9]
                      [10;11;12]]
    let A5 = Matrix.ofArray2D a5
    let R5 = Matrix.square A5
    printfn "//--------------------"
    printfn "origin = \n %A" A5.values
    printfn "square = \n %A" R5.values

    //--------------------
    let a6 = array2D [[1;2;3]
                      [4;5;6]
                      [7;8;9]
                      [10;11;12]]
    let A6 = Matrix.ofArray2D a6
    let R6 = Matrix.T A6
    printfn "//--------------------"
    printfn "origin = \n %A" A6.values
    printfn "transpose = \n %A" R6.values

    //--------------------
    let a7 = array2D [[1;2;3]
                      [4;5;6]
                      [7;8;9]
                      [10;11;12]]
    let A7 = Matrix.ofArray2D a7
    let R7 = Matrix.sizes A7
    printfn "//--------------------"
    printfn "origin = \n %A" A7.values
    printfn "rows %d cols %d" (fst R7) (snd R7)

    //--------------------
    let a81 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]
                       [10;11;12]]
    let a82 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]
                       [10;11;12]]

    let A81 = Matrix.ofArray2D a81
    let A82 = Matrix.ofArray2D a82

    let R81 = Matrix.isEquallySized A81 A82

    printfn "//--------------------"
    printfn "origin1 = \n %A" A81.values
    printfn "origin1 = \n %A" A82.values

    printfn "is equally sized = \n %A" R81

    let a83 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]]
    let a84 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]
                       [10;11;12]]

    let A83 = Matrix.ofArray2D a83
    let A84 = Matrix.ofArray2D a84

    let R82 = Matrix.isEquallySized A83 A84

    printfn "//--------------------"
    printfn "origin1 = \n %A" A83.values
    printfn "origin2 = \n %A" A84.values

    printfn "is equally sized = \n %A" R82

    //--------------------
    let a91 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]
                       [10;11;12]]
    let a92 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]
                       [10;11;12]]

    let A91 = Matrix.ofArray2D a91
    let A92 = Matrix.ofArray2D a92

    let R91 = A91 == A92

    printfn "//--------------------"
    printfn "origin1 = \n %A" A91.values
    printfn "origin2 = \n %A" A92.values

    printfn "is same = \n %A" R91

    let a93 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]]
    let a94 = array2D [[1;2;3]
                       [4;5;6]
                       [7;8;9]
                       [10;11;12]]

    let A93 = Matrix.ofArray2D a93
    let A94 = Matrix.ofArray2D a94

    let R92 = A93 == A94

    printfn "//--------------------"
    printfn "origin1 = \n %A" A93.values
    printfn "origin2 = \n %A" A94.values

    printfn "is same = \n %A" R92