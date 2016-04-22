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
tamanio file = (*8).length.contenido $ file

-- OK, pero yo usaría composición // corregido

--archivo está vacío
esvacio :: Archivo -> Bool
esvacio file = null.contenido $ file

-- OK pero usaría composición // corregido 

--cantidad de líneas de un archivo
cantidadlineas :: Archivo -> Int
cantidadlineas file = length.lines.contenido $ file

-- IDEM ANTERIOR

--líneas del archivo es blanca
lineablanca :: Archivo -> Bool
lineablanca file = (any (\x -> all (isSpace) x)).lines.contenido $ file

-- IDEM ANTERIOR // corregido

--archivo es de extensión .hs
extensionhs :: Archivo -> Bool
extensionhs file = (==".hs").(intersect ".hs").nombre $ file

-- IDEM ANTERIOR // corregido

--Renombrar un archivo
type Modificacion = Archivo -> Archivo

renombrararchivo :: String -> Archivo -> Archivo
renombrararchivo name (Archivo a b) = Archivo name b

-- OK

--Agregar una nueva línea al archivo
agregarlinea :: Archivo -> Int -> String -> Archivo
agregarlinea file numero linea = Archivo (nombre file) (tePongoLaLinea (lines.contenido $ file) numero linea)
tePongoLaLinea lineaFile numero linea = unlines ((take numero lineaFile) ++ (lines linea) ++ (drop numero lineaFile))

-- Si la respuesta es UFFFFF es porque falta delegar con buenos nombres
-- Delegar a funciones auxiliares para que sea mas declarativo (cajita negra que le paso algo y devuelve algo y no me interesa como esta hecho)
-- y para que sea mas expresivo  // corregido

--Quitar una línea del archivo
quitarlinea :: Archivo -> Int -> Archivo
quitarlinea file numero = Archivo (nombre file) (teSacoLaLinea (lines.contenido $ file) numero)
teSacoLaLinea lineaFile numero = unlines ((take (numero - 1) lineaFile)  ++ (drop numero lineaFile))

-- IDEM ANTERIOR (agregarLinea) // corregido

--Reemplazar una línea del archivo por otra
reemplazarlinea :: Archivo -> Int -> String -> Archivo
reemplazarlinea file numero linea = Archivo (nombre file) (teCambioLaLinea (lines.contenido $ file) numero linea)
teCambioLaLinea lineaFile numero linea = unlines ((take (numero - 1) lineaFile) ++ (lines linea) ++ (drop numero lineaFile))

-- IDEM ANTERIOR (agregarLinea) // corregido

--Buscar y reemplazar en el archivo
buscarreemplazar :: Archivo -> String -> String -> Archivo
buscarreemplazar file palabra nueva = Archivo (nombre file) (unlines (map (\x -> if (all (isSpace) x) then x else unwords (map (\x -> if x == palabra then nueva else x) (words x))) (lines (contenido file))))

-- IDEM ANTERIOR (agregarLinea)

--Wrappear las líneas del archivo
wrappeararchivo :: Archivo -> Archivo
cortarlinea :: String -> [String]
wrappear' :: [String] -> [String]

wrappeararchivo file = Archivo (nombre file) (unlines.wrappear'.lines.contenido $ file)
wrappear' [] = []
wrappear' (linea:restolinea) = cortarlinea linea ++ wrappear' restolinea
cortarlinea "" = [""]
cortarlinea linea = take 4 linea : cortarlinea (drop 4 linea) 

--Saber si una modificación es inútil
esnecesario :: Archivo -> String -> String
esnecesario file content | (==content).contenido $ file = "Es una modificación inútil"
                         | otherwise = "Modificacion necesaria"

--lines . contenido $ wrappeararchivo unTpGrupal 

--Aplicar una revisión sobre un archivo
revision :: Archivo -> [String]
revision file = lines.contenido $ file
