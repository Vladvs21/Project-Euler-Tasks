module Vakulenko where

-- Problem 1 (Multiples of 3 or 5) --
sum1000 :: Int
sum1000 = foldl1 (\x y -> if (y `mod` 3 == 0 || y `mod` 5 == 0) then x + y else x) [0..999]

-- Problem 2 (Even Fibonacci numbers) --
sumFib = foldl1 (\x y -> if (y `mod` 2 == 0 ) then x + y else x) (takeWhile (<=4000000) (map fib [0..]))

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Problem 3 (Largest prime factor) --
largestPrimeFactor = maximum (prime_factors 600851475143)

prime_factors n = let factors = take 1 (filter (\x -> (n `mod` x) == 0) [2 .. n-1])
                  in if null factors
                     then [n]
                     else factors ++ prime_factors (n `div` (head factors))

-- Problem 4 (Largest palindrome product) --
largestPal = maximum (filter (\y -> (reverse (show y)) == show y) (map product [[x,y] | x<-[100..999], y<-[100..999]]))

-- Problem 5 (Smallest multiple) --
smallestMult = foldl1 (*) (map (\y -> power 1 y 20) (filter (\x -> (factors x) == [1, x]) [1..20]))

factors n = filter (\x -> n `mod` x == 0) [1..n]

power v i limit | v * i < limit = power (v * i) i limit
                | otherwise = v

-- Problem 6 (Sum square difference) --
sumSqDif = ((foldl1 (+) [1..100]) ** 2) - (foldl1 (+) (map (\x -> x ** 2) [1..100]))

-- Problem 7 (10001st prime) --
prime10001st = last (take 10001 (filter (\x -> (factors x) == [1, x]) (2:[3,5..])))

-- Problem 8 (Largest product in a series) --
largProdSer = maximum (listOfProds 0)

listOfProds o | o == 1000 - 12 = []
              | otherwise = (foldl1 (*) (take 13 (drop o numList))) : listOfProds (o + 1)

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

numList = digs num

-- Problem 9 (Special Pythagorean triplet) --
triangles = take 1 [(a,b,c) | c <- [1..], b <- [1..c], a <- [1..b] , a^2 + b^2 == c^2, a+b+c == 1000]

-- Problem 10 (Summation of primes) --
sumOfPrimes = foldl1 (+) (takeWhile (<=2000000) (filter (\x -> (factors x) == [1, x]) (2:[3,5..])))