import Text.Show.Functions ()

type Nombre = String
type Kilometraje = Int
type Condicion = Viaje -> Bool

type Ubicacion = String

type Fecha = (Int,Int,Int)
type Costo = Int

type Cliente = (Nombre,Ubicacion)

-- 1.

data Chofer = UnChofer { nombreDelChofer :: Nombre,
                         kilometrajeDelAuto :: Kilometraje,
                         viajesQueTomo :: [Viaje],
                         condicionParaTomarUnViaje :: Condicion
                       } deriving (Show)

data Viaje = UnViaje { fecha :: Fecha,
                       cliente :: Cliente,
                       costo :: Costo
                     } deriving (Show)

-- Auxiliares - Cliente

nombreDelCliente :: Cliente -> Nombre
nombreDelCliente = fst

ubicacionDelCliente :: Cliente -> Ubicacion
ubicacionDelCliente = snd

-- Auxiliares - Viaje

nombreDelClienteDeUnViaje :: Viaje -> Nombre
nombreDelClienteDeUnViaje unViaje = nombreDelCliente.cliente $ unViaje

ubicacionDelClienteDeUnViaje :: Viaje -> Ubicacion
ubicacionDelClienteDeUnViaje unViaje = ubicacionDelCliente.cliente $ unViaje

-- Auxliares - Chofer

listaDeCostosDelViajeQueTomo :: Chofer -> [Costo]
listaDeCostosDelViajeQueTomo unChofer = map costo.viajesQueTomo $ unChofer

cantidadDeViajesQueTomo :: Chofer -> Int
cantidadDeViajesQueTomo unChofer = length.viajesQueTomo $ unChofer

cambiarViajes :: ([Viaje] -> [Viaje]) -> Chofer -> Chofer
cambiarViajes unaFuncion unChofer = unChofer {viajesQueTomo = unaFuncion.viajesQueTomo $ unChofer}

-- 2. 
-- Condiciones

tomaCualquierViaje :: Condicion
tomaCualquierViaje _ = True

viajeMasCaroQue200 :: Condicion
viajeMasCaroQue200 unViaje = costo unViaje > 200

clienteConMasDeNLetras :: Int -> Condicion
clienteConMasDeNLetras cantidadDeLetras unViaje = cantidadDeLetrasDelNombreDelCliente unViaje > cantidadDeLetras

cantidadDeLetrasDelNombreDelCliente :: Viaje -> Int
cantidadDeLetrasDelNombreDelCliente unViaje = length (nombreDelClienteDeUnViaje unViaje)

noViveEn :: Ubicacion -> Condicion
noViveEn unBarrio unViaje = not ((ubicacionDelClienteDeUnViaje unViaje) == unBarrio)


-- 3.
-- Ejemplos

lucas :: Cliente
lucas = ("Lucas","Victoria")

daniel :: Chofer
daniel = UnChofer "Daniel" 23500 [lucasTrip] (noViveEn "Olivos")

alejandra :: Chofer
alejandra = UnChofer "Alejandra" 180000 [] tomaCualquierViaje

lucasTrip :: Viaje
lucasTrip = UnViaje (20,4,2017) lucas 150

-- 4.

puedeTomarUnViaje :: Chofer -> Condicion
puedeTomarUnViaje = condicionParaTomarUnViaje 

puedeTomarUnViaje' :: Viaje -> Chofer -> Bool
puedeTomarUnViaje' unViaje unChofer = condicionParaTomarUnViaje unChofer $ unViaje

-- 5.

liquidacionDeUnChofer :: Chofer -> Int
liquidacionDeUnChofer unChofer = sum (listaDeCostosDelViajeQueTomo unChofer)

-- 6.

realizarUnViaje :: Viaje -> [Chofer] -> Chofer
realizarUnViaje unViaje listaDeChoferes = agregarViaje unViaje.choferQueTieneMenosViajes unViaje $ listaDeChoferes

-- a)

choferesQueTomanElViaje :: Viaje -> [Chofer] -> [Chofer]
choferesQueTomanElViaje unViaje listaDeChoferes = filter (puedeTomarUnViaje' unViaje) $ listaDeChoferes

-- b)

choferQueTieneMenosViajes :: Viaje -> [Chofer] -> Chofer
choferQueTieneMenosViajes unViaje listaDeChoferes = foldl1 quienTieneMenosViajes. choferesQueTomanElViaje unViaje $ listaDeChoferes 

quienTieneMenosViajes :: Chofer -> Chofer -> Chofer
quienTieneMenosViajes unChofer otroChofer
 | cantidadDeViajesQueTomo unChofer < cantidadDeViajesQueTomo otroChofer = unChofer
 | otherwise = otroChofer

-- c)

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje unViaje = cambiarViajes (unViaje :) 

-- 7.
-- a)

nitoInfy :: Chofer
nitoInfy = UnChofer "Nito Infy" 70000 infinitosViajesConLucas (clienteConMasDeNLetras 3)

viajesConLucas :: Viaje
viajesConLucas = UnViaje (11,3,2017) lucas 50

infinitosViajesConLucas :: [Viaje]
infinitosViajesConLucas = repeat viajesConLucas

-- b)

{-
No puedo calcular la liquidacion de Nito Infy ya que utilizo funciones como
map o sum que deben evaluar todos los parametros para devolver un resultado,
lo cual es imposible hacer con una lista infinita sin que se quede colgado. 
-}

-- c)

{-
Es posible saber si Nito va a tomar un viaje con Lucas de $500 ya que la 
condicion de Nito Infy para tomar un viaje es simplemente ver la cantidad 
de letras del nombre del cliente, no necesita evaluar la lista entera.
-}





