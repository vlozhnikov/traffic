namespace fTrafficCore

module Algorithms =
    
    let markingOfConnectedComponents matrix =
        // up to 100 markers
        let parents = Array2D.init 100 2 (fun c r -> [if r = 0 then c+1 else 0])
        
        let rec find x (parents: int[,]) = 
            let index = Array.findIndex ((=)x) parents.[1,*]
            match parents.[0, index] with
            | p when p <> 0 -> find p parents
            | _ -> x

        let union x y parents =
            let j = find x parents
            let k = find y parents
            if j <> k then parents.[1,k] <- j

        Matrix.O 1 1