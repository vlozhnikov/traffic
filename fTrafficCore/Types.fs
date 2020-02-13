namespace fTrafficCore

module Types =

    type Position = { x: float; y: float; z: float }

    type Node =
        | Empty
        | CrossRoad
        | TrafficLight

    type Point = { p: Position; t: Node }
    let point p t =
        { p = p; t = t }

    type Road = { n: string; pl: Point list }
        with
            member r.append po =
                { n=r.n; pl=po::r.pl }
        end

    let road n pl =
        { n = n; pl = pl }