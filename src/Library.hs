module Library where
import PdePreludat

type Precio = Chocolate -> Number

data Chocolate = Chocolate {
    nombre          :: String,
    gramaje         :: Number,
    azucar          :: Number,
    cacao           :: Number,
    caloriasTotales :: Number,
    ingredientes    :: [Ingrediente]
} deriving (Eq, Show)

data Ingrediente = Ingrediente {
    sabor    :: String,
    calorias :: Number
} deriving (Eq, Show)

-- Chocolates
milkaOreo :: Chocolate
milkaOreo = Chocolate "Milka Oreo" 100 50 40 36 [Ingrediente "Leche" 20, Ingrediente "Banana" 8, Ingrediente "Muzzarella" 8]

-- Punto 1
aptoParaDiabeticos :: Chocolate -> Bool
aptoParaDiabeticos = (==0) . azucar 

esAmargo :: Chocolate -> Bool
esAmargo chocolate = ((>60) . cacao) chocolate && (elem "Cacao" . map sabor) (ingredientes chocolate)

cantidadDeIngredientes :: Chocolate -> Number
cantidadDeIngredientes = length . ingredientes

precioPremium :: Precio
precioPremium chocolate 
  | aptoParaDiabeticos chocolate = ((*8) . gramaje) chocolate
  | otherwise                    = ((*5) . gramaje) chocolate

precioDelChocolate :: Precio
precioDelChocolate chocolate 
  | esAmargo chocolate                   = gramaje chocolate * precioPremium chocolate
  | cantidadDeIngredientes chocolate > 4 = 8 * cantidadDeIngredientes chocolate 
  | otherwise                            = 1.5 * gramaje chocolate

-- Punto 2
esBombonAsesino :: Chocolate -> Bool
esBombonAsesino chocolate = any ((>200) . calorias) (ingredientes chocolate)

totalCalorias :: Chocolate -> Number
totalCalorias chocolate = (sum . map calorias) (ingredientes chocolate)

aptoParaNinios :: [Chocolate] -> [Chocolate]
aptoParaNinios = (take 3 . filter esBombonAsesino)

-- Punto 2 parte 2
agregarIngrediente :: Ingrediente -> Chocolate -> Chocolate
agregarIngrediente ingrediente chocolate = chocolate {
    ingredientes = ingrediente : ingredientes chocolate,
    caloriasTotales = caloriasTotales chocolate + calorias ingrediente 
}

nuevoNombreTentacion :: Chocolate -> Chocolate
nuevoNombreTentacion chocolate = chocolate { nombre = nombre chocolate ++ " tentación"}

frutalizado :: Chocolate -> String -> Number -> Chocolate
frutalizado chocolate nombreFruta cantidad = agregarIngrediente (Ingrediente nombreFruta (cantidad * 2)) chocolate 

dulceDeLeche :: Chocolate -> Chocolate
dulceDeLeche = nuevoNombreTentacion . agregarIngrediente (Ingrediente "Dulce de leche" 220)

celiaCrucera :: Chocolate -> Number -> Chocolate
celiaCrucera chocolate porcentaje = chocolate { azucar = azucar chocolate + porcentaje }

-- embriagadora

-- Punto 4
