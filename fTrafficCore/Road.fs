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
                |> List.toSeq

            // returns crossroad points of road's list
            static member CrossRoads (roads: Road list) =

                let rec inCross r roads res = 
                    match roads with
                    | h::t when h.n <> r.n ->
                        Road.CrossRoads2 r h
                            |> Seq.append res
                            |> inCross r t
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

    // road's graph
    type Edge = { u: Vertex; v: Vertex }
    type Graph = { vertexs: seq<Vertex>; lines: seq<Edge>; }
        with

            // returns true if graph is empty
            member g.IsEmpty = 
                if (Seq.length g.vertexs) = 0 || (Seq.length g.lines) = 0 then true
                else false

            // initilize new graph by list of roads
            static member fromRoads roads =

                // returns roads by vertex
                let roadsByVertex v rl = 
                    rl
                    |> List.filter (fun r ->
                                        let f = List.filter (fun p -> p.p == v) r.pl
                                        if f.Length > 0 then true
                                        else false
                                   )

                // returns micrographs by vertex and roads
                let micrographByVertexRoads v rl =
                    rl
                    |> List.map (fun r ->
                                    let ind0 = List.findIndex (fun f -> f.p == v) r.pl
                                    match ind0 with
                                    | x when x > 0 && x < r.pl.Length - 1 ->
                                        { vertexs = [v]; lines = [{u = v; v = r.pl.[ind0-1].p}; {u = v; v = r.pl.[ind0+1].p}] }
                                    | x when x > 0 && x = r.pl.Length - 1 ->
                                        { vertexs = [v]; lines = [{u = v; v = r.pl.[ind0-1].p}] }
                                    | x when x = 0 ->
                                        { vertexs = [v]; lines = [{u = v; v = r.pl.[ind0+1].p}] }
                                    | _ -> { vertexs = []; lines = [] } 
                                )
                    |> List.filter (fun f -> if f.IsEmpty then false else true)

                // find all possibilities vertexs
                let vertexs = Road.CrossRoads roads |> Seq.map (fun v -> v.p)

                // make seq of micrographs
                let micrographs = vertexs
                                    |> Seq.map (fun v ->
                                                    let rv = roadsByVertex v roads
                                                    let gl = micrographByVertexRoads v rv
                                                    gl
                                               )
                // uncomment for printing of micrographs
                //micrographs |> Seq.iter (fun gl ->
                            //    printfn "MICRO GRAPHS"
                            //    for g in gl do
                            //        printfn "--------------------"
                            //        printfn "VERTEXS"
                            //        for ver in g.vertexs do
                            //            printfn "vertex x = %f, y = %f" ver.x ver.y
                            //        printfn "LINES"
                            //        for lin in g.lines do
                            //            printfn "u x = %f, y = %f, v x = %f y = %f" lin.u.x lin.u.y lin.v.x lin.v.y
                            //)

                // assymbly the graph from micrographs

                // make the graph
                let graph = { vertexs = vertexs; lines = Seq.empty }
                graph

        end