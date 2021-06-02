-- Prática 04 de Haskell
-- Nome: Brenda Emanuelle Scherer

--1
faixaIdoso :: Int -> String
faixaIdoso age
          | age>=60 && age<=64 = "IDO64"
          | age>=65 && age<=69 = "IDO69"
          | age>=70 && age<=74 = "IDO74"
          | age>=75 && age<=79 = "IDO79"
          | age>=80 = "IDO80"
          | otherwise = "erro"

--2
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos pessoa = [(name,age,faixaIdoso age) | (name,age) <- pessoa]

--3
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' pessoa = map (\(name,age) -> (name,age,faixaIdoso age)) pessoa

--4
strColor :: (Int,Int,Int) -> String
strColor rgb = "rgb" ++ show rgb

--5
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n ponto r = [(pontox,snd ponto,r) | pontox <- [fst ponto, fst ponto + 2*r ..  fst ponto + 2*(n-1)*r]]

--6 
--se o valor passar de 255, que é o valor rgb, ele retorna 404, indicando o erro
genReds :: Int -> [(Int,Int,Int)]
genReds ncores = if ncores*10+70 <= 250 then[(r,0,0) | r <- [ 80, 80 + 10 .. 80 + 10*ncores-1]] else [(404,404,404)]