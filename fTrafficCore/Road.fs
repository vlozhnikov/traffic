namespace fTrafficCore

module Road =

    [<Measure>]
    type m // metr

    // ---------------------------------------------

    // coordinate in the map
    type Vertex = { x: float; y: float; z: float }
        with
            // returns true if two points are equal
            static member (==) (p1, p2) =
                p1.x = p2.x && p1.y = p2.y && p1.z = p2.z
            static member (!=) (p1, p2) =
                p1.x <> p2.x || p1.y <> p2.y || p1.z <> p2.z
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
    type Point = { p: Vertex; t: Node list }
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
                |> Set.ofSeq

            // returns crossroad points of road's list
            static member CrossRoads (roads: List<Road>) =

                let rec inCross r roads res = 
                    match roads with
                    | h::t when h.n <> r.n ->
                        Road.CrossRoads2 r h
                            |> Set.union res
                            |> inCross r t
                    | _::t ->
                        inCross r t res
                    | [] -> res

                roads
                |> List.map (fun x -> inCross x roads Set.empty)
                |> List.collect (fun i -> List.ofSeq i)
                |> List.distinct

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

    // road's graph
    type Edge = { u: Vertex; v: Vertex }
    type Graph = { vertexs: Set<Vertex>; lines: Set<Edge>; }
        with

            // returns true if graph is empty
            member g.IsEmpty = 
                if (Set.count g.vertexs) = 0 || (Set.count g.lines) = 0 then true
                else false

            // initilize new graph by list of roads
            static member fromRoads roads =

                let correspondenceMatrix (vs: List<Vertex>) (rs: Set<Road>) = 

                    let matrix = Array2D.init vs.Length vs.Length (fun _ _ -> (0))

                    let inMatrix row col (r: Road) =

                        let vr = vs.[row]
                        let vc = vs.[col]
                        let f1 = r.TryFindPoint vr
                        let f2 = r.TryFindPoint vc

                        match (f1, f2) with
                        | (Some(x), Some(y)) ->
                                                let i1 = List.findIndex (fun (f: Point) -> f.p == x.p) r.pl
                                                let i2 = List.findIndex (fun (f: Point) -> f.p == y.p) r.pl

                                                if abs(i1 - i2) <= 1 then
                                                    matrix.[row, col] <- 1
                        | _ -> ()

                    for r in 0..vs.Length-1 do
                        for c in 0..vs.Length-1 do
                            if r <> c then
                                rs |> Set.iter (fun i -> inMatrix r c i)

                    matrix

                // find all possibilities vertexs
                let vertexs = Road.CrossRoads roads |> List.map (fun f -> f.p)
                let res = correspondenceMatrix vertexs (Set.ofList roads)

                printfn "%A" res

                // make the graph
                let graph = { vertexs = Set.empty; lines = Set.empty }
                graph

        end