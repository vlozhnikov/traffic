namespace fTrafficCore

module Types =

    [<Measure>]
    type m // metr

    // ---------------------------------------------

    // coordinate in the map
    type Position = { x: float; y: float; z: float }
        with
            // returns true if two points are equal
            static member (==) (p1, p2) =
                p1.x = p2.x && p1.y = p2.y && p1.z = p2.z
        end
    let position (x, y, z) =
        { x=x; y=y; z=z }

    // ---------------------------------------------

    // type of the node on the map. uses inside point
    type Node =
        | BEGIN // start point of road (optional)
        | END // end point of road(optional)
        | BREAK // break of road (optional)
        | CONTINUE // continue of road (optional)
        | COORDINATE
        | TRAFFICLIGHT

    // point on the map. uses inside roads, etc
    type Point = { p: Position; t: Node list }
    let point (x, y, z) t =
        { p = position (x, y, z); t = t }

    // ---------------------------------------------

    // returns 'Cross' if list of points contains some point 
    let (|Includes|DoesntIncludes|) (point, list) = 
        let tryPoint = List.tryFind (fun c2 ->
                                        if point == c2.p then
                                            true
                                        else
                                            false) list
        match tryPoint with
            | Some(x) -> Includes x
            | _ -> DoesntIncludes

    // this is a road
    type Road = { n: string; pl: Point list }
        with
            // static member to append new point and returns new road
            static member (+) (r, po) =
                { n=r.n; pl=r.pl @ [po] }

            // static member finds public crossroads of road1 and road2
            static member CrossRoads2 r1 r2 =

                r1.pl
                |> List.filter (fun c1 -> match (c1.p, r2.pl) with
                                          | Includes(_) -> true
                                          | _ -> false
                               )

            static member CrossRoads (roads: Road list) =

                let rec inCross r roads res = 
                    match roads with
                    | h::t when h.n <> r.n ->
                        let nextRes = Seq.append res (Road.CrossRoads2 r h)
                        inCross r t nextRes
                    | _::t ->
                        inCross r t res
                    | [] -> res

                roads
                |> Seq.map (fun x -> inCross x roads Seq.empty)
                |> Seq.collect (fun r -> r)
                |> Seq.distinct

            // returns total length of road
            // sqrt((x2 - x2)^2 + (y2 - y1)^2 + (z2 - z1)^2)
            member r.Length =
                let rec inLength rest l =
                    match rest with
                    | _::[]
                    | [] -> l
                    | h::t ->
                        let nextHead = List.head t
                        let nextLength = l + sqrt(pown (h.p.x - nextHead.p.x) 2 + pown (h.p.y - nextHead.p.y) 2 + pown (h.p.z - nextHead.p.z) 2)
                        inLength t nextLength

                let coordinates = List.filter (fun x -> List.contains COORDINATE x.t) r.pl
                inLength coordinates 0.

            // try find point on the road by position
            member r.TryFindPoint pos = match (pos, r.pl) with
                                            | Includes(x) -> Some(x)
                                            | _ -> None
                
        end

    let road n pl =
        { n = n; pl = pl }