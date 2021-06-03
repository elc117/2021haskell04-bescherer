import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0)]

-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (150,150)
        gap = 0

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

svgCircle :: Circle -> String -> String 
svgCircle ((x,y),r) style = 
  printf "<circle x='%50.8f' y='%.6f' r='%.2f' style='%s' />\n" x y r style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgRect [((225,460),30,30),((255,460),30,30),((195,430),30,30),((225,430),30,30),((255,430),30,30),((285,430),30,30),((165,400),30,30),((195,400),30,30),((225,400),30,30),((255,400),30,30),((285,400),30,30),((315,400),30,30),((135,370),30,30),((165,370),30,30),((195,370),30,30),((225,370),30,30),((255,370),30,30),((285,370),30,30),((315,370),30,30),((345,370),30,30),((105,340),30,30),((135,340),30,30),((165,340),30,30),((195,340),30,30),((225,340),30,30),((255,340),30,30),((285,340),30,30),((315,340),30,30),((345,340),30,30),((375,340),30,30),((75,310),30,30),((105,310),30,30),((135,310),30,30),((165,310),30,30),((195,310),30,30),((225,310),30,30),((255,310),30,30),((285,310),30,30),((315,310),30,30),((345,310),30,30),((375,310),30,30),((405,310),30,30),((45,280),30,30),((75,280),30,30),((105,280),30,30),((135,280),30,30),((165,280),30,30),((195,280),30,30),((225,280),30,30),((255,280),30,30),((285,280),30,30),((315,280),30,30),((345,280),30,30),((375,280),30,30),((405,280),30,30),((435,280),30,30),((45,250),30,30),((75,250),30,30),((105,250),30,30),((135,250),30,30),((165,250),30,30),((195,250),30,30),((225,250),30,30),((255,250),30,30),((285,250),30,30),((315,250),30,30),((345,250),30,30),((375,250),30,30),((405,250),30,30),((435,250),30,30),((45,220),30,30),((75,220),30,30),((105,220),30,30),((135,220),30,30),((165,220),30,30),((195,220),30,30),((225,220),30,30),((255,220),30,30),((285,220),30,30),((315,220),30,30),((345,220),30,30),((375,220),30,30),((405,220),30,30),((435,220),30,30),((45,190),30,30),((75,190),30,30),((105,190),30,30),((135,190),30,30),((165,190),30,30),((195,190),45,30),((285,190),45,30),((315,190),30,30),((345,190),30,30),((375,190),30,30),((405,190),30,30),((435,190),30,30),((75,160),30,30),((105,160),30,30),((135,160),30,30),((165,160),45,30),((315,160),45,30),((345,160),30,30),((375,160),30,30),((405,160),30,30),((105,130),45,30),((135,130),45,30),((345,130),45,30),((375,130),30,30)] (map svgStyle palette)
        palette = rgbPalette nrects
        nrects = 200
        (w,h) = (500,500) -- width,height da imagem SVG