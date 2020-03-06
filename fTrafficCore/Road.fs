namespace fTrafficCore

module Road =

    [<Measure>]
    type m // metr

    // ---------------------------------------------

    // coordinate in the map
    type Point = { x: float; y: float }
        with
            // returns true if two points are equal
            static member (==) (p1, p2) =
                p1.x = p2.x && p1.y = p2.y
            static member (!=) (p1, p2) =
                p1.x <> p2.x || p1.y <> p2.y
        end
    let point (x, y) =
        { x=x; y=y }

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
                        let nextLength = l + sqrt(pown (h.x - nextHead.x) 2 + pown (h.y - nextHead.y) 2(* + pown (h.z - nextHead.z) 2*))
                        inLength t nextLength

                inLength r.points 0.
                
        end

    let road n pl =
        { name = n; points = pl }

    // road's graph
    type Edge = { u: Point; v: Point }
    type Graph = { vertexs: Set<Point>; edges: Set<Edge>; }
        with

            static member ofRoads roads =

                let makeMatrix (vertexs: Point list) (roads: Road list) = 

                    let matrix = Array2D.init vertexs.Length vertexs.Length (fun _ _ -> (0))

                    let inMatrix row col road =

                        let vertexFrom = vertexs.[row]
                        let vertexTo = vertexs.[col]

                        let u = match (vertexFrom, road.points) with
                                    | Contains(x) -> Some(x)
                                    | _ -> None
                        let v = match (vertexTo, road.points) with
                                    | Contains(x) -> Some(x)
                                    | _ -> None

                        match (u, v) with
                        | (Some(x), Some(y)) ->
                                let iu = List.findIndex ((==) x) road.points
                                let iv = List.findIndex ((==) y) road.points

                                let isAnother = [(System.Math.Min (iu, iv) + 1)..(System.Math.Max (iu, iv) - 1)]
                                                |> List.exists (fun i ->
                                                                    (List.exists ((==) road.points.[i]) vertexs))

                                if not isAnother then
                                     matrix.[row, col] <- 1
                        | _ -> ()

                    matrix
                    |> Array2D.iteri (fun col row i ->
                                        roads
                                        |> List.iter (fun road ->
                                                        if row <> col then 
                                                            inMatrix row col road)
                                     )

                    matrix

                // get vertexs
                let vertexs = Road.Vertexs roads
                let res = makeMatrix vertexs roads

                printfn "%A" res
        end