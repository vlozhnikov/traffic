namespace fTrafficCore

module Algorithms =
    
    let subMatrix row col width height matrix =
        let (rows, cols) = Matrix.sizes matrix
        if rows < (row + height) || cols < (col + width) then failwith "submatrix is out of range"
        let values = matrix.values.[row..(row+height-1), col..(col+width-1)]
        {values = values}