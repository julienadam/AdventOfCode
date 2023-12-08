namespace AdventOfCode

module MathEx =
    
    let rec gcd x y = if y = 0 then abs x else gcd y (x % y)
    let lcm x y = x * y / (gcd x y)

    let rec gcd64 x y = if y = 0L then abs x else gcd64 y (x % y)
    let lcm64 x y = x * y / (gcd64 x y)