struct Sequence E
    Array E array
    Int     size
    Int     start
    Int     end

seq (Int n) = sequence (array (n), n, 0, n)
