module Library where
import PdePreludat

--Punto 1: Numeros positivos

--positivo :: Ord a => a => a
--positivo 
--        | x < 0 == True = False
--    	| x > 0 == False = True

{-
a. Marcar errores que hacen que directamente no funcione correctamente

- El tipo de la función está mal, debería ser:
positivo :: Number -> Bool

- Falta definir el parámetro x antes de las guardas
- Las condiciones x < 0 y x > 0 deberían ir entre paréntesis para poder compararlas con True y False en cada rama
- La función se llama "positivo", sin embargo si se usa un número positivo, no sabe decirnos si es positivo o no (no entra por ninguna rama de la guarda, ambas evaluan la misma condición).
- No considera el 0 en ninguna rama
-}

--La función compila y hace lo que dice el nombre si hacemos esto:
positivo' :: Number -> Bool
positivo' x | (x <= 0) == True = False
            | (x > 0) == True = True

{-
b. Indicar cosas que si bien funcionan, se pueden hacer mejor

Hay un uso innecesario de booleanos, por un lado no es necesario hacer el == True en ninguna de las dos ramas, dado que la expresión x<=0 y la expresión
x > 0 tienen un valor booleano (se reducen a un True o False), por ende si por ejemplo:

x > 0 es True, hacer (x > 0) == True devuelve True
x > 0 es False, hacer (x > 0) == True devuelve False

Asi que es lo mismo que solo hacer x > 0, el codigo podría ser:
-}

positivo'' :: Number -> Bool
positivo'' x | (x <= 0) = False
             | (x > 0) = True

{-
Sin embargo, esa solución tampoco es correcta y hace mal uso de guardas y de booleanos. Con la misma idea, se cumple que:

x > 0 con un número positivo, da True
x > 0 con un número no positivo, da False

Puedo directamente no usar guardas y usar la condición x > 0
-}

--c. Implementar una versión corregida

positivo :: Number -> Bool
positivo x = x > 0

--También podría ser:

positivo2 :: Number -> Bool
positivo2 = (>0)

--Punto 2: Costos de entrada

costoEntrada :: Number -> Number
costoEntrada edad
   | edad > 18 = 100 + 2 * (edad + 1)
   | edad > 60 = 100 + 2 * (edad / 2 - 20)
   | otherwise = 100 + 2 * (18 - edad)

--Solucion mejorada

costoEntrada' :: Number -> Number
costoEntrada' edad = 100 + 2 * (multiplicadorSegun edad)

multiplicadorSegun :: Number -> Number
multiplicadorSegun edad | edad > 18 = edad + 1
                        | otherwise = 18 - edad

{-
b. Justificar brevemente por qué la nueva versión es mejor.

La nueva versión evita la repetición de lógica en el cálculo de la entrada, dado que siempre sin importar la edad se hace 100 + 2 * (algo que depende de la edad)
Si el día de mañana me dicen que ahora la base en lugar de ser 100 es 150, lo cambio en un solo lugar, no en tres.

Por otro lado, para que la función tenga exactamente el mismo comportamiento, se omite el segundo caso de la guarda original (el caso de edad > 60), ya que
por cómo está hecha, si la edad es mayor a 60, también cumple ser mayor a 18, por ende entra en la primera rama.

Si queremos considerar el caso de que edades mayores a 60 tienen diferente multiplicador deberíamos hacer:
-}

multiplicadorSegun' :: Number -> Number
multiplicadorSegun' edad | edad > 60 = edad / 2 - 20
                         | edad > 18 = edad + 1
                         | otherwise = 18 - edad

{-
Al invertir el orden, si la edad es mayor a 60 entra por la primera rama, si no cumple recién va a la segunda, por ende la segunda rama 
es las edades mayores a 18 y a la vez menores a 60.
Si no se cumple tampoco la segunda rama, es decir la edad es menor o igual 18, entro al ultimo caso
-}

--Punto 3: Devuelve algo

devuelveAlgo x y z w  
        | x y && x z = w y + z
        | otherwise  = z

devuelveOtraCosa a b c = (> a) . length . filter b . map c

-- Inferir el tipo mas genérico de las funciones.

{-

Para devuelveAlgo podemos arrancar pensando cuantas cosas recibe para ver cuantos tipos necesitamos. Como tiene x y z w como parámetros, sabemos que tiene
4 parametros y devuelve una cosa:

x -> y -> z -> w -> m

Podemos ver que hay una suma en la primera rama de la guarda, y como el tipo de la función (+) es: Number -> Number -> Number podemos saber de w y + z que:

- z es un Number
- Lo que devuelve la función es un Number
- (w y) es un Number, por ende probablemente w sea una función que al aplicarla con un parámetro del mismo tipo de y, me devuelve un Number

Tenemos entonces:

x -> y -> Number -> (y -> Number) -> Number

Luego, sabiendo que el tipo de la función (&&) es Bool -> Bool -> Bool sabemos que:

- (x y) es un Bool, por ende x es una función que al aplicarle un parámetro del tipo de y, devuelve un Bool
- (x z) también es un bool y x sigue siendo la misma función, si le puedo aplicar tanto z como y, entonces tanto z como y son del mismo tipo

Podemos inferir entonces que:

(y -> Bool) -> y -> Number -> (y -> Number) -> Number

Y sabiendo que y tiene el mismo tipo que z y que z es un Number, entonces:
-}

devuelveAlgo :: (Number -> Bool) -> Number -> Number -> (Number -> Number) -> Number

{-

Con devuelveOtraCosa podemos primero ver cuantos parámetros tiene, podemos ver que recibe a b c, por lo que minimamente tiene 3 parámetros. Pero además
podemos ver que la composición entera no está siendo aplicada a algo, por lo que devuelveOtraCosa minimamente devuelve una función de un parámetro:

a -> b -> c -> (d -> e)

Como la primera función que se aplica (la de más a la derecha) es map c, como el tipo de map es (x -> y) -> [x] -> [y]
podemos inferir que:

- c es una función que va de (x -> y)
- d (lo que recibe la función que resulta de toda la composición) es una lista de x

Entonces: 
a -> b -> (x -> y) -> ([x] -> e)

De filter b, sabiendo que el tipo de filter es (z -> Bool) -> [z] -> [z], y además sabiendo que lo que resulte del map c es lo que va a recibir el filter
podemos inferir que:

- b es una función que va de z a Bool
- la lista que recibe el filter es el mismo tipo que devuelve el map, por ende z es del mismo tipo que y

Entonces:

a -> (y -> Bool) -> (x -> y) -> ([x] -> e)

Del length, sabiendo que length siempre devuelve un Number, y que lo que devuelve es lo que va a recibir la función (>a)
podemos inferir:

- a es un Numero
- Si comparo dos numeros en (>a), la función me termina devolviendo un Bool, entonces e es un Bool

Entonces: 
-}

devuelveOtraCosa :: Number -> (y -> Bool) -> (x -> y) -> ([x] -> Bool)

{-
b. Para cada función, mostrar al menos dos ejemplos de llamada con elementos de diferente tipo,
en caso que la función permita diferentes tipos, o un único ejemplo en caso contrario, y el resultado que daría en cada uno. 

devuelveAlgo even 5 6 (+2)
> 6

devuelveOtraCosa 6 even (*3) [1, 2, 3]
> False

devuelveOtraCosa 10 (elem 'b') ("c"++) ["a", "b", "c"]
> False

-}

--Punto 4: Mayores

-- SOLO HACER PUNTO a, NO ENTRA TESTING EN EL PARCIAL!!!!!

{-
Teniendo esta funcion junto con lo que devuelve en diferentes casos:

mayoresQueNAlAplicar 4 (\x -> x) [1,2,3,4,5,6,7]
>[5, 6, 7]

mayoresQueNAlAplicar 3 (+1) [1,2,3,4,5]
>[3, 4, 5]

mayoresQueNAlAplicar 4 negate [4,5,6,6000]
> []

Definir la función
-}

mayoresQueNAlAplicar :: Number -> (Number -> Number) -> [Number] -> [Number]
mayoresQueNAlAplicar numero funcion = filter (\x -> funcion x > numero)

--Punto 5: Votación


data Mesa = UnaMesa {
    distrito:: String,
    votos :: [Partido],
    cantidadVotantesHabilitados::Number
} deriving Show


{-
Existe la función maximoSegun que recibe un transformador y devuelve el máximo de la lista. 

maximoSegun abs [4,2,-3,-10,7]
> -10 
-}

-- El parcial no pide definirla pero podría ser así:
maximoSegun :: Ord a => (b -> a) -> [b] -> b
maximoSegun _ [x] = x
maximoSegun funcion (x:y:xs) | funcion x > funcion y = maximoSegun funcion (x:xs)
                             | otherwise = maximoSegun funcion (y:xs)

-- O con foldl1
maximoSegun' :: Ord a => (b -> a) -> [b] -> b
maximoSegun' funcion = foldl1 (elMayorSegun funcion)

elMayorSegun :: Ord a => (b -> a) -> b -> b -> b
elMayorSegun funcion x y | funcion x > funcion y = x
                         | otherwise = y



--a. Obtener al partido ganador de una elección

type Partido = String

data Eleccion = UnaEleccion {
    mesas :: [Mesa],
    partidos :: [Partido]
}


votosEnMesaDe :: Partido -> Mesa -> Number
votosEnMesaDe partido = length.filter (==partido).votos

votosTotalesDe :: Partido -> Eleccion -> Number
votosTotalesDe partido = sum.map (votosEnMesaDe partido).mesas

ganador :: Eleccion -> Partido
ganador eleccion = maximoSegun (flip votosTotalesDe eleccion) (partidos eleccion)


-- b. Determinar si fue una elección irregular, que sucede cuando hay mesas donde la cantidad de votos emitidos supera a la cantidad de votantes habilitados.

fueIrregular :: Eleccion -> Bool
fueIrregular = any esMesaIrregular.mesas

esMesaIrregular :: Mesa -> Bool
esMesaIrregular mesa = cantidadDeVotos mesa > cantidadVotantesHabilitados mesa

--Uso cantidadDeVotosSegun con id porque no quiero aplicarles ningun cambio a los votos (por ejempl)
cantidadDeVotos :: Mesa -> Number
cantidadDeVotos = length.votos

--c. Obtener la mesa con el menor índice de ausentismo.

mesaConMenorAusentismo :: Eleccion -> Mesa
mesaConMenorAusentismo = maximoSegun ausentismo.mesas

ausentismo :: Mesa -> Number
ausentismo mesa = cantidadVotantesHabilitados mesa - cantidadDeVotos mesa 

--Ejemplos de mesas y elecciones

mesaDelA = UnaMesa {
    distrito = "Un distrito",
    votos = ["PartidoA","PartidoA","PartidoA","PartidoB", "PartidoC"],
    cantidadVotantesHabilitados = 5
}

mesaDelB = UnaMesa {
    distrito = "Otro distrito",
    votos = ["PartidoA","PartidoA","PartidoB","PartidoB", "PartidoB"],
    cantidadVotantesHabilitados = 4
}

eleccion = UnaEleccion {
    mesas = [mesaDelA, mesaDelB],
    partidos = ["PartidoA", "PartidoB", "PartidoC"]
}

{-
ganador eleccion
> "PartidoA"

fueIrregular eleccion
> True

mesaConMenorAusentismo eleccion
> UnaMesa {distrito = "Un distrito", votos = ["PartidoA","PartidoA","PartidoA","PartidoB","PartidoC"], cantidadVotantesHabilitados = 5}

-}

{-
d. ¿Qué pasaría si hubiera un lista infinitas de mesas? ¿Qué puntos funcionarían y qué puntos no? Justificar

Un ejemplo de una eleccion con infinitas mesas sería:
-}

eleccionInfinita = UnaEleccion {
    mesas = repeat mesaDelB,
    partidos = ["PartidoA", "PartidoB", "PartidoC"]
}

{-
- Para el caso de ganador y mesaConMenorAusentismo, al usar maximoSegun que necesita evaluar todos los elementos de la lista, a pesar de tener evaluación diferida
se quedaría colgado con mesas infinitas.
- Para el caso de fueIrregular, la función andaría únicamente para el caso de que una de las mesas de la elección cumpla con ser irregular. Gracias 
a la evaluación diferida de haskell y que la función use un any, con que un solo elemento cumpla la función es suficiente para que corte con la ejecución.
Sin embargo si ninguna mesa cumple con ser irregular, se colgaría evaluando infinitamente las mesas.
-}