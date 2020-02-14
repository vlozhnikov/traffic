// Learn more about F# at http://fsharp.org

open System
open fTrafficCore.Types

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let road1 =
        road "road1" [
                        point (6., 0., 0.) [Coordinate; CrossRoad];
                        point (9., 0., 0.) [Coordinate; TrafficLight];
                        point (9., 6., 0.) [Coordinate; CrossRoad];
                    ]

    let road2 =
        road "road2" [
                        point (6., 0., 0.) [Coordinate; CrossRoad];
                        point (9., 1., 0.) [Coordinate];
                        point (9., 6., 0.) [Coordinate; TrafficLight; CrossRoad];
                    ]

    let crossrods = road1.TryFindPoint (position (6., 1., 0.))

    printfn "road length road1 %f, road2 %f" road1.Length road2.Length

    0 // return an integer exit code
