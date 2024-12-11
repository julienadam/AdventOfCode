namespace AdventOfCode

module MathEx =
    
    let rec gcd x y = if y = 0 then abs x else gcd y (x % y)
    let lcm x y = x * y / (gcd x y)

    let rec gcd64 x y = if y = 0L then abs x else gcd64 y (x % y)
    let lcm64 x y = x * y / (gcd64 x y)

    /// hardcoding for the win
    let pows64 = 
        [
        1L;
        10L;
        100L;
        1000L;
        10000L;
        100000L;
        1000000L;
        10000000L;
        100000000L;
        1000000000L;
        10000000000L;
        100000000000L;
        1000000000000L;
        10000000000000L;
        100000000000000L;
        1000000000000000L;
        10000000000000000L;
        100000000000000000L;
        1000000000000000000L
    ]

    let pow10 n = pows64[n]