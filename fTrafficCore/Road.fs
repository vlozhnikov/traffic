namespace fTrafficCore

module Road =

    [<Measure>]
    type m // metr

    // ---------------------------------------------

    // coordinate in the map
    type Point = { x: float; y: float; z: float }
        with
            // returns true if two points are equal
            static member (==) (p1, p2) =
                p1.x = p2.x && p1.y = p2.y && p1.z = p2.z
            static member (!=) (p1, p2) =
                p1.x <> p2.x || p1.y <> p2.y || p1.z <> p2.z
        end
    let point (x, y, z) =
        { x=x; y=y; z=z }

    // ---------------------------------------------

    // returns 'Includes' if list of points contains some point 
    let (|Includes|DoesntIncludes|) (point, list) = 
        let tryPoint = List.tryFind (fun p ->
                                        if point == p then true
                                        else false) list
        match tryPoint with
            | Some(x) -> Includes x
            | _ -> DoesntIncludes

    // this is a road
    type Road = { name: string; points: Point list }
        with
            // static member to append new point and returns new road
            static member (+) (r, po) =
                { name=r.name; points=r.points @ [po] }

            // static member finds public crossroads of road1 and road2
            static member CrossRoads2 r1 r2 =

                r1.points
                |> List.filter (fun c1 -> match (c1, r2.points) with
                                          | Includes(_) -> true
                                          | _ -> false
                               )
                |> Set.ofSeq

            // returns crossroad points of road's list
            static member CrossRoads (roads: List<Road>) =

                let rec inCross r roads res = 
                    match roads with
                    | h::t when h.name <> r.name ->
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
                        let nextLength = l + sqrt(pown (h.x - nextHead.x) 2 + pown (h.y - nextHead.y) 2 + pown (h.z - nextHead.z) 2)
                        inLength t nextLength

                inLength r.points 0.

            // try find point on the road by position
            member r.TryFindPoint pos = match (pos, r.points) with
                                            | Includes(x) -> Some(x)
                                            | _ -> None
                
        end

    let road n pl =
        { name = n; points = pl }

    // road's graph
    type Edge = { u: Point; v: Point }
    type Graph = { vertexs: Set<Point>; edges: Set<Edge>; }
        with

            // returns true if graph is empty
            member g.IsEmpty = 
                if (Set.count g.vertexs) = 0 || (Set.count g.edges) = 0 then true
                else false

            // initilize new graph by list of roads
            static member fromRoads roads =

                let correspondenceMatrix (vs: List<Point>) (rs: Set<Road>) = 

                    let matrix = Array2D.init vs.Length vs.Length (fun _ _ -> (0))

                    let inMatrix row col (r: Road) =

                        let vr = vs.[row]
                        let vc = vs.[col]
                        let f1 = r.TryFindPoint vr
                        let f2 = r.TryFindPoint vc

                        match (f1, f2) with
                        | (Some(x), Some(y)) ->
                                                let i1 = List.findIndex (fun f -> f == x) r.points
                                                let i2 = List.findIndex (fun f -> f == y) r.points

                                                if abs(i1 - i2) <= 1 then
                                                    matrix.[row, col] <- 1
                        | _ -> ()

                    for r in 0..vs.Length-1 do
                        for c in 0..vs.Length-1 do
                            if r <> c then
                                rs |> Set.iter (fun i -> inMatrix r c i)

                    matrix

                // find all possibilities vertexs
                let vertexs = Road.CrossRoads roads
                let res = correspondenceMatrix vertexs (Set.ofList roads)

                printfn "%A" res

                // make the graph
                let graph = { vertexs = Set.empty; edges = Set.empty }
                graph

        end