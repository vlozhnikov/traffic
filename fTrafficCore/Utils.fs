namespace fTrafficCore

module Utils =

    // example of usage
    // let rec f =
    //     memoize (
    //         fun n -> ...
    //     )
    let memoize (f: 'a -> 'b) =
        let t = new System.Collections.Generic.Dictionary<'a, 'b>()
        fun n -> 
            if t.ContainsKey(n) then t.[n]
            else
                let res = f n
                t.Add(n, res)
                res