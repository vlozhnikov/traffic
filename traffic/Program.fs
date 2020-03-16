// Learn more about F# at http://fsharp.org

open System
open fTrafficCore
open fTrafficCore.Road
open TerminalView


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    (*let rh1 =
        road "rh1" [
                        point (0., 7.);
                        point (1., 7.);
                        point (2., 7.);
                        point (3., 7.);
                        point (4., 7.);
                        point (5., 7.);
                        point (6., 7.);
                        point (7., 7.);
                        point (8., 7.);
                    ]

    let rh2 =
        road "rh2" [
                        point (0., 5.);
                        point (1., 5.);
                        point (2., 5.);
                        point (3., 5.)
                        point (4., 5.);
                        point (5., 5.);
                        point (6., 5.);
                        point (7., 5.);
                        point (8., 5.);
                    ]

    let rh3 =
        road "rh3" [
                        point (0., 3.);
                        point (1., 3.);
                        point (2., 3.);
                        point (3., 3.);
                        point (4., 3.);
                        point (5., 3.);
                        point (6., 3.);
                        point (7., 3.);
                        point (8., 3.);
                    ]

    let rh4 =
        road "rh4" [
                        point (0., 1.);
                        point (1., 1.);
                        point (2., 1.);
                        point (3., 1.);
                        point (4., 1.);
                        point (5., 1.);
                        point (6., 1.);
                        point (7., 1.);
                        point (8., 1.);
                    ]

    let rv1 =
        road "rv1" [
                        point (3., 8.);
                        point (3., 7.);
                        point (3., 6.);
                        point (3., 5.);
                        point (3., 4.);
                        point (3., 3.);
                        point (3., 2.);
                        point (3., 1.);
                        point (3., 0.);
                    ]

    let rv2 =
        road "rv2" [
                        point (5., 8.);
                        point (5., 7.);
                        point (5., 6.);
                        point (5., 5.);
                        point (5., 4.);
                        point (5., 3.);
                        point (5., 2.);
                        point (5., 1.);
                        point (5., 0.);
                    ]

    let rv3 =
        road "rv3" [
                        point (7., 8.);
                        point (7., 7.);
                        point (7., 6.);
                        point (7., 5.);
                        point (7., 4.);
                        point (7., 3.);
                        point (7., 2.);
                        point (7., 1.);
                        point (7., 0.);
                    ]

    let roads = [rh1; rh2; rh3; rh4; rv1; rv2; rv3]*)
    //let crossrods = Road.CrossRoads roads

    let r1 =
        road "R1" [
                        point (1., 1.);
                        point (1., 2.);
                        point (1., 3.);
                        point (2., 4.);
                        point (2., 5.);
                        point (3., 6.);
                        point (4., 6.);
                        point (5., 5.);
                        point (5., 4.);
                        point (6., 3.);
                        point (5., 2.);
                        point (4., 2.);
                        point (3., 1.);
                    ]

    let r2 =
        road "R2" [
                        point (0., 5.);
                        point (1., 5.);
                        point (2., 5.);
                        point (3., 5.);
                        point (4., 5.);
                        point (5., 5.);
                        point (6., 5.);
                        point (7., 5.);
                    ]

    let r3 =
        road "R3" [
                        point (4., 3.);
                        point (5., 2.);
                        point (6., 1.);
                    ]

    let roads = [r1; r2; r3]

    //crossrods
    //|> Seq.iter (fun x -> printfn "position x: %f, y: %f" x.p.x x.p.y)

    //let g = Graph.ofRoads roads

    //let terminal = new TerminalView()
    //terminal.Width <- Array2D.length1 g
    //terminal.Height <- Array2D.length2 g
    //terminal.Fps <- 30

    //terminal.Start()

    // test matrixs 

    let zeroMatrix = Matrix.zero 5 5
    printfn "%A" zeroMatrix

    let unitMatrix = Matrix.unit 5 5
    printfn "%A" unitMatrix

    let array2d = array2D [
                            [01;02;03;04;05;06]
                            [07;08;09;10;11;12]
                            [13;14;15;16;17;18]
                            [19;20;21;22;23;24]
                          ]

    let matrix = Matrix.ofArray2D array2d
    let triangular = Matrix.triangular matrix
    let diagonal = Matrix.diagonal matrix
    let square = Matrix.square triangular

    printfn "%A" triangular.values
    printfn "%A" diagonal.values
    printfn "%A" square.values

    0 // return an integer exit code
