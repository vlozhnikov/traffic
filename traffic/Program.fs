// Learn more about F# at http://fsharp.org

open System
open fTrafficCore.Road

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let rh1 =
        road "rh1" [
                        point (1., 7., 0.) [BEGIN; COORDINATE];
                        point (3., 7., 0.) [COORDINATE];
                        point (5., 7., 0.) [COORDINATE];
                        point (7., 7., 0.) [COORDINATE];
                        point (8., 7., 0.) [COORDINATE; TRAFFICLIGHT; END];
                    ]

    let rh2 =
        road "rh2" [
                        point (1., 5., 0.) [BEGIN; COORDINATE];
                        point (3., 5., 0.) [COORDINATE];
                        point (5., 5., 0.) [COORDINATE];
                        point (7., 5., 0.) [COORDINATE];
                        point (8., 5., 0.) [COORDINATE; TRAFFICLIGHT; END];
                    ]

    let rh3 =
        road "rh3" [
                        point (1., 3., 0.) [BEGIN; COORDINATE];
                        point (3., 3., 0.) [COORDINATE];
                        point (5., 3., 0.) [COORDINATE];
                        point (7., 3., 0.) [COORDINATE];
                        point (8., 3., 0.) [COORDINATE; TRAFFICLIGHT; END];
                    ]

    let rh4 =
        road "rh4" [
                        point (1., 1., 0.) [BEGIN; COORDINATE];
                        point (3., 1., 0.) [COORDINATE];
                        point (5., 1., 0.) [COORDINATE];
                        point (7., 1., 0.) [COORDINATE];
                        point (8., 1., 0.) [COORDINATE; TRAFFICLIGHT; END];
                    ]

    let rv1 =
        road "rv1" [
                        point (3., 8., 0.) [BEGIN; COORDINATE];
                        point (3., 7., 0.) [COORDINATE];
                        point (3., 5., 0.) [COORDINATE];
                        point (3., 3., 0.) [COORDINATE];
                        point (3., 1., 0.) [COORDINATE];
                        point (3., 0., 0.) [END; COORDINATE; TRAFFICLIGHT];
                    ]

    let rv2 =
        road "rv2" [
                        point (5., 8., 0.) [BEGIN; COORDINATE];
                        point (5., 7., 0.) [COORDINATE];
                        point (5., 5., 0.) [COORDINATE];
                        point (5., 3., 0.) [COORDINATE];
                        point (5., 1., 0.) [COORDINATE];
                        point (5., 0., 0.) [END; COORDINATE; TRAFFICLIGHT];
                    ]

    let rv3 =
        road "rv3" [
                        point (7., 8., 0.) [BEGIN; COORDINATE];
                        point (7., 7., 0.) [COORDINATE];
                        point (7., 5., 0.) [COORDINATE];
                        point (7., 3., 0.) [COORDINATE];
                        point (7., 1., 0.) [COORDINATE];
                        point (7., 0., 0.) [END; COORDINATE; TRAFFICLIGHT];
                    ]

    let length = rh1.Length
    let crossrods = Road.CrossRoads [rh1; rh2; rh3; rh4; rv1; rv2; rv3]

    crossrods
    |> Seq.iter (fun x -> printfn "position x: %f, y: %f" x.p.x x.p.y)

    0 // return an integer exit code
