import System.Random
import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import System.Environment   


randInRange :: Int -> Int -> IO Int
randInRange a b = getStdRandom $ randomR (a, b)

imprimePrimo :: Int -> Bool
imprimePrimo n = (divisores n n) == 2

divisores :: Int -> Int -> Int
divisores n 1 = 1
divisores n m  = if  mod n m == 0 then 1 +  divisores n (m-1)
               else divisores n (m -1)
               
divisorescop :: Int -> Int -> Int -> Int
divisorescop phi_n m 1 = 1
divisorescop phi_n m p = 
	if mod phi_n p == 0 && mod m p == 0 
	then 1 + divisorescop phi_n m (p - 1)
	else divisorescop phi_n m (p - 1)
               
randomPrimo :: IO Int
randomPrimo = do 
	x <- randInRange 50 100
	if imprimePrimo x then return x else randomPrimo
				
coprimo :: Int -> IO Int
coprimo phi_n = do
	m <- randInRange 10 20
	if divisorescop phi_n m m == 1 then return m
	else coprimo phi_n
	
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then mkPos i else 0
			 where mkPos x = if x < 0 then x + m else x
			 
encriptar :: Int -> (Int, Int) -> Integer
encriptar m (e, n) = mod (toInteger m ^ toInteger e) (toInteger n)

desencriptar :: Integer -> (Int, Int) -> Integer
desencriptar c (d, n) = mod  (toInteger c ^ toInteger d) (toInteger n)

probarq :: Int -> IO Int
probarq p = do
	q <- randomPrimo
	if q == p then probarq p else return q
			
abrirArchivo :: String -> IO String
abrirArchivo filename = do
		handle <- openFile filename ReadMode  
		contents <- hGetContents handle
		return contents

encriptarArchivo :: String -> Int -> Int -> Int -> String -> IO()
encriptarArchivo contenido e n i salida = do
		let largo = length contenido
		if i < largo then do
			let ch = contenido !! i
			let codigo = (ord ch)
			let c = encriptar codigo (e, n)
			let s = show c ++ " "
			encriptarArchivo contenido e n (i+1) (salida ++ s)
		else do
	        writeFile "encriptado.txt" salida
					
desencriptarArchivo :: String -> Int -> Int -> Int -> String -> IO()
desencriptarArchivo contenido d n i salida = do
		let codigos = splitOn " " contenido
		let largo = length codigos
		if i < largo - 1 then do
			let codigo = codigos !! i
			let codigo' = read  codigo :: Integer
			let m = desencriptar codigo' (d, n)
			let m' = read $ show m :: Int
			let c = chr m'
			desencriptarArchivo contenido d n (i+1) (salida ++ [c])
			
		else 
			writeFile "desencriptado.txt" (show salida)

generarClaves :: [String] -> IO()
generarClaves n = do
	p <- randomPrimo
	q <- probarq p
	let n = p * q
	let phi_n = (p-1) * (q-1)
	e <- coprimo phi_n
	let d = modInv e phi_n
	putStrLn $ "llave publica: " ++ show e ++ "," ++ show n
	putStrLn $ "llave privada: " ++ show d ++ "," ++ show n

	
funcEncriptar :: [String] -> IO()
funcEncriptar (path:x:y:xs) = do
	contenido <- abrirArchivo path
	let e = read x :: Int
	let n = read y :: Int
	encriptarArchivo contenido e n 0 ""
	putStrLn "mensaje encriptado"

funcDesencriptar :: [String] -> IO()
funcDesencriptar (path:x:y:xs) = do
	contenido <- abrirArchivo path
	let d = read x :: Int
	let n = read y :: Int
	desencriptarArchivo contenido d n 0 ""
	putStrLn "mensaje desencriptado"

dispatch :: [(String, [String] -> IO ())]
dispatch = [("encriptar",funcEncriptar),
			("desencriptar",funcDesencriptar),
			("generarclaves", generarClaves)
			]
		
main = do 
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args
