import Data.Char
import Data.List

data Archivo = Archivo { nombre :: String, contenido :: String } deriving (Show, Eq)
-- ejemplo
unTpGrupal :: Archivo
unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n"
testFile :: Archivo
testFile = Archivo "tesths" " palabra inicio ... palabra\n           \n... fin"

--Para ejecutar :l tpgrupal
--tamaño de un archivo en bytes
tamanio :: Archivo -> Int
tamanio file = (*8) (length (contenido file))
--archivo está vacío
esvacio :: Archivo -> Bool
esvacio file = null (contenido file)
--cantidad de líneas de un archivo
cantidadlineas :: Archivo -> Int
cantidadlineas file = length (lines (contenido file))
--líneas del archivo es blanca
lineablanca :: Archivo -> Bool
lineablanca file = any (\x -> all (isSpace) x) (lines (contenido file))
--archivo es de extensión .hs
extensionhs :: Archivo -> Bool
extensionhs file = (intersect  ".hs" (nombre file)) == ".hs" --en revision
--Renombrar un archivo
renombrararchivo :: Archivo -> String -> Archivo
renombrararchivo (Archivo a b) name = Archivo name b
--Agregar una nueva línea al archivo
agregarlinea :: Archivo -> Int -> String -> Archivo
agregarlinea file numero linea = Archivo (nombre file) (unlines ((take numero (lines (contenido file))) ++ [linea] ++ (drop numero (lines (contenido file)))))
--Quitar una línea del archivo
quitarlinea :: Archivo -> Int -> Archivo
quitarlinea file numero = Archivo (nombre file) (unlines ((take ((-) numero 1) (lines (contenido file)))  ++ (drop numero (lines (contenido file)))))
--Reemplazar una línea del archivo por otra
reemplazarlinea :: Archivo -> Int -> String -> Archivo
reemplazarlinea file numero linea = Archivo (nombre file) (unlines ((take ((-) numero 1) (lines (contenido file))) ++ [linea] ++ (drop numero (lines (contenido file)))))
--Buscar y reemplazar en el archivo
buscarreemplazar :: Archivo -> String -> String -> Archivo
buscarreemplazar file palabra nueva = Archivo (nombre file) (unlines (map (\x -> if (all (isSpace) x) then x else unwords (map (\x -> if x == palabra then nueva else x) (words x))) (lines (contenido file))))
--Wrappear las líneas del archivo
wrappeararchivo :: Archivo -> Archivo
wrappeararchivo file = Archivo "" "" --map (\n x -> if (length x) < 80 then x else ) (lines (contenido file))
--Saber si una modificación es inútil
esnecesario :: Archivo -> String -> String
esnecesario file content | (contenido file) == content = "Es una modificacion inutil"
						 | otherwise = "Modificacion necesaria"
