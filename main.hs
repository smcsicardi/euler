fib :: Int -> Int
fib 0 = 1
fib 1 = 2
fib a = fib (a-1) + fib (a-2)

superFib :: Int -> Int -> Int
superFib a x | (a <= (fib x)) = 0
			 | otherwise = (fib x) + superFib a (x+3)

fact :: Integer -> [Integer]
fact a = factAux a 2

factAux :: Integer -> Integer -> [Integer]
factAux 1 _ = []
factAux a x | (a==x) = [a]
		 | a `mod` x == 0 = x:(factAux (a `div` x)  x)
		 | otherwise	  = factAux a (x+1) 

maxLista :: [Integer] -> Integer
maxLista [] = 0
maxLista [x] = x
maxLista (x:(y:xs)) | (x >= y) = maxLista (x:xs)
		            | otherwise = maxLista (y:xs)

digs :: Integer -> [Integer]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

esPalindromo :: Integer -> Bool
esPalindromo x | reverse (digs x) == digs x = True
			   | otherwise = False

mayorPalindromo :: Integer -> Integer -> [Integer]
mayorPalindromo 1000 _ = []
mayorPalindromo x y | (y==999)&&(esPalindromo (x*y))  =(x*y):(mayorPalindromo (x+1) 100) 
					| (y==999) = (mayorPalindromo (x+1) 100)
					| esPalindromo (x*y) = (x*y):(mayorPalindromo x (y+1))
					| otherwise =  mayorPalindromo x (y+1)

sumaCuadrados :: Int -> [Int]
sumaCuadrados 0 = []
sumaCuadrados x = (x*x) : sumaCuadrados (x-1)

cuadradosSuma :: Int -> Int -> Int
cuadradosSuma x y = cuadradosSuma (x-1) (y+x)
cuadradosSuma 1 y = y

isPrime k = null [ x | x <- [2..k-1], k `mod`x  == 0]

listaPrimos :: Integer -> [Integer]
listaPrimos x | isPrime x = (listaPrimos (x-1))++[x]
				| otherwise = listaPrimos (x-1)

sieve (x:xs) = x : sieve (filter ((/= 0) . (`mod` x)) xs)

auxiliar :: Integer -> Integer -> Integer -> [(Integer, Integer, Integer)]
auxiliar a b c | (a*a+b*b==c*c) = (a,b,c):(auxiliar a b (c-1))
			   | otherwise = auxiliar a b (c-1)
auxiliar a b 0 = auxiliar a (b-1) 1000
auxiliar a 0 0 = auxiliar (a-1) 1000 1000
auxiliar 0 0 0 = []

pitagorico :: [(Integer, Integer, Integer)] -> Integer
pitagorico ((a, b, c):xs) | a+b+c==1000 = a*b*c
						|otherwise = pitagorico xs
