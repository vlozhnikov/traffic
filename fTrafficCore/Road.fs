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

    // returns 'Contains' if set of points contains the point 
    let (|Contains|DoesntContains|) (point, points) = 
        let isExists = List.exists (fun p ->
                                        if point == p then true
                                        else false) points
        if isExists then Contains point
        else DoesntContains

    // this is a road
    type Road = { name: string; points: Point list }
        with
            static member Vertexs roads =

                let res = List.fold (fun acc elem ->
                                       let filtered = List.filter (fun f -> f.name <> elem.name) roads
                                       List.fold (fun acc2 elem2 ->
                                                     let points =
                                                         List.filter (fun p ->
                                                                         match (p, elem2.points) with
                                                                         | Contains(x) -> not (List.exists ((==) x) acc)
                                                                         | _ -> false
                                                         ) elem.points
                                                     acc2 @ points
                                       ) acc filtered
                           ) [] roads

                res |> List.iteri (fun i f -> printfn "vertex %d x: %f, y: %f" (i+1) f.x f.y)
                res

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
                                            | Contains(x) -> Some(x)
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

                let makeMatrix (vs: List<Point>) (rs: Set<Road>) = 

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
                let vertexs = Road.Vertexs roads
                let res = makeMatrix vertexs (Set.ofList roads)

                printfn "%A" res

                // make the graph
                let graph = { vertexs = Set.empty; edges = Set.empty }
                graph

        end