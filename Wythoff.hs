import System.IO ( IO, hFlush, stdout, getLine, putChar, putStrLn )
import Prelude
import System.Random ( randomRIO )
import System.Process


inicio = do
            putStrLn "¿Qué desea hacer?"  -- [Texto del menú]
            putStrLn "(1) Jugar una partida"  -- [Opción 1 del menú]
            putStrLn "(2) Ver instrucciones"  -- [Opción 2 del menú]
            putStrLn "(3) Salir del juego"  -- [Opción 3 del menú]
            imprimir "\n"  -- [Salto de línea]
            imprimir "Selecciona una opción: "  -- [Texto del menú]
            hFlush stdout  -- [Ordena la memoria para mostrar el texto en pantalla y luego permitir el ingreso]*(1)
            eleccion <- getLine  -- [Guarda en eleccion la entrada numérica]
            imprimir "\n"  -- [Salto de línea]
            let elec = read eleccion :: Int -- [Guarda en elec los datos ingresados como tipo int]
            if elec <= 0 || elec > 3 then putStrLn ("Numero invalido" ++ "\n") else imprimir ""
            if elec <= 0 || elec > 3 then inicio else do return elec
            return elec {- [En estas lineas se analiza si el valor ingresado es distinto a las opciones. En caso de que
                            sea diferente, se avisa en pantalla que el número ingresado es inválido y se vuelve a la
                            función "inicio". En caso de que el valor ingresado sea correcto, se devuelve el valor
                            ingresado guardado en "elec".]-}


ingresoMax :: String -> IO Int  -- [Aplica mónada]
ingresoMax nombre1= do  imprimir "\n"  --[Imprime un salto de línea]
                        imprimir "Elija el numero maximo por fila: " -- [Pide ingresar el largo máximo de cada fila]
                        hFlush stdout  -- [Ordena la memoria para mostrar el texto en pantalla y luego permitir el ingreso]*(1)
                        maximo <- getLine  -- [Guarda en maximo la entrada númerica]
                        system "cls"  -- [Función q limpia la pantalla]
                        let max = read maximo :: Int  --[Guarda en max los datos ingresados como tipo Int]
                        if max <= 1  then putStrLn ("Numero invalido" ++ "\n") else imprimir ""  --[Si el valor es menor o igual a 1, imprime que el valor es inválido]
                        if max <= 1 then ingresoMax nombre1 else do return max  --[Si el valor es menor o igual a 1, vuelve a la función ingresoMax]


imprimir [ ] = return ()
imprimir (x : xs) = do
                        putChar x
                        imprimir xs   {- [Esta función permite imprimir una cadena de cáracteres usando
                        Pattern-Matching. Imprime el primer caracter y luego llama a imprimir con la "cola" de la cadena]-}


dif x xs z =  do
    y <- randomRIO(1, z)
    if x == xs then dif x y z else return (x, xs)   {- [Esta función permite generar un par de números aleatorios distintos entre sí.
                                                    Primero le asigna un valor aleatorio entre 1 y z. Si x y xs son iguales, llama a dif
                                                    con x e y, sino devuelve x y xs.] -}

dibujarFila x =do if x /= 0 then "*"++dibujarFila (x-1) else []   -- [Crea las filas de (*). Verifica si x es diferente de 0, si es así concatena un asterisco al valor de x-1. Si es 0 devuelve nil]

gano jug nombre1 nombre2 = if jug then putStrLn ("Ganó " ++ nombre1) else putStrLn ("Ganó " ++ nombre2)   --[Imprime el ganador del juego. Si jug es verdadero, gano el jugador1, caso contrario ganó el jugador 2. Imprime "Ganó" el nombre del ganador]

jugar fila1 fila2 jug nombre1 nombre2 = do  imprimir "\n"
                                            let lista1 = dibujarFila fila1  --[Guarda en lista1 una cadena de (*) de largo fila1]
                                            let lista2 = dibujarFila fila2  --[Guarda en lista1 una cadena de (*) de largo fila1]
                                            putStrLn ("1 : " ++ lista1)  --[Imprime la primer fila de (*)]
                                            putStrLn ("2 : " ++ lista2)  --[Imprime la segunda fila de (*)]
                                            if jug then imprimir nombre1 else imprimir nombre2  --[Si jug es true, imprime el nombre del jugador1, caso contrario imprime el nombre del jugador2. Se usa para indicar el jugador de turno]
                                            imprimir " saca de fila 1: " --[Texto de la interfaz. Pregunta cuantos asteriscos va a sacar de la fila 1]
                                            hFlush stdout  -- [Ordena la memoria para mostrar el texto en pantalla y luego permitir el ingreso]*(1)
                                            i1 <- getLine  -- [Guarda en il la entrada númerica]
                                            let input1 = read i1 :: Int  --[Guarda en input1 los datos ingresados como tipo Int]
                                            if fila1 < input1 || input1 < 0 then putStrLn ("Jugada invalida" ++ "\n") else imprimir ""  --[Si input1 es menor a 0 o mayor a fila1, se indica que es una jugada ivalida. Sino, se imprime ""]
                                            if fila1 < input1 || input1 < 0 then jugar fila1 fila2 (not jug) nombre1 nombre2 else do
                                                                                                                                    if jug then imprimir nombre1 else imprimir nombre2  --[Si jug es true, imprime el nombre del jugador1, caso contrario imprime el nombre del jugador2. Se usa para indicar el jugador de turno]
                                                                                                                                    imprimir " saca de fila 2: "
                                                                                                                                    hFlush stdout
                                                                                                                                    i2 <- getLine
                                                                                                                                    let input2 = read i2 :: Int
                                                                                                                                    if fila2 < input2 || input2 < 0 then putStrLn ("Jugada invalida" ++ "\n") else imprimir ""
                                                                                                                                    if fila2 < input2 || input2 < 0 then jugar fila1 fila2 (not jug) nombre1 nombre2 else do
                                                                                                                                                                                                                            if ((input1 == input2) && (input1 == 0)) then putStrLn ("Movimiento inválido, no se puede saltar el turno"++"\n") else imprimir ""
                                                                                                                                                                                                                            if ((input1 == input2) && (input1 == 0)) then jugar fila1 fila2 jug nombre1 nombre2 else do
                                                                                                                                                                                                                                                                                                                        if ((input1 /= input2) && (input1 /= 0)) && (input2 /=0) then putStrLn ("No se puede eliminar distintas cantidades de las filas"++"\n") else imprimir ""
                                                                                                                                                                                                                                                                                                                        if ((input1 /= input2) && (input1 /= 0)) && (input2 /=0) then jugar fila1 fila2 (not jug) nombre1 nombre2 else do
                                                                                                                                                                                                                                                                                                                                                                                                                                            let fila_1 = fila1 - input1
                                                                                                                                                                                                                                                                                                                                                                                                                                            let fila_2 = fila2 - input2
                                                                                                                                                                                                                                                                                                                                                                                                                                            if fila_1 == fila_2 && fila_1 == 0 then return jug else jugar fila_1 fila_2 (not jug) nombre1 nombre2
                                                                                                                                    {-[En estas filas se analiza si el valor ingresados son válidos. No se pueden ingresar un valor menor al de la fila correspondiente. No se pueden ingresar valores diferentes en el caso de que se quiten (*) a ambas filas. No se puede pasar turno ingresando un 0 en ambas opciones.
                                                                                                                                    Si sucede alguno de estos casos, se vuelve a llamar a la función jugar (repite turno). Si todas las condiciones se cumplen, se resta los valores ingresados de los valores númericos de las filas. Si ambos valores quedan en 0, devuelve jug, caso contrario se llama a la función jugar con (not jug). (Se pasa al turno del otro jugador)]-}



main = do
          system "cls"
          putStrLn "BIENVENIDO A WYTHOFF"  --[Título del juego]
          elegir <- inicio  --[Guarda el valor de inicio en elegir]
          case elegir of 1 -> do  --[Si elegir es 1]
                                system "cls"  -- [Función q limpia la pantalla]
                                imprimir "Seleccione un nombre para el jugador 1: "  --[Texto del menú]
                                hFlush stdout  -- [Ordena la memoria para mostrar el texto en pantalla y luego permitir el ingreso]*(1)
                                nombre1 <- getLine  --[Guarda en nombre1 la cadena ingresada]
                                imprimir "\n"  --[Salto de línea]
                                imprimir "Seleccione un nombre para el jugador 2: " --[Texto del menú]
                                hFlush stdout  -- [Ordena la memoria para mostrar el texto en pantalla y luego permitir el ingreso]*(1)
                                nombre2 <- getLine  --[Guarda en nombre2 la cadena ingresada]
                                imprimir "\n"  --[Salto de línea]
                                xs <- ingresoMax nombre1  --[Guarda en xs el valor de ingresoMax nombre1]
                                fila1 <- randomRIO(1, xs) :: IO Int -- [Guarda en fila1 el valor de randomRIO(1, xs) de tipo IO Int]
                                fila2 <- randomRIO(1, xs) :: IO Int -- [Guarda en fila2 el valor de randomRIO(1, xs) de tipo IO Int]
                                (y, x) <- dif fila1 fila2 xs   -- [Guarda en la tupla (y,x) el resultado de dif fila1 fila2 xs]
                                let fila1 = y  -- [Asigna a fila1 el valor de y]
                                let fila2 = x  -- [Asigna a fila2 el valor de x]
                                jug <- jugar fila1 fila2 True nombre1 nombre2   --[Guarda en jug el resultado de jugar fila1 fila2 True nombre1 nombre2]
                                imprimir "\n"  -- [Salto de línea]
                                gano jug nombre1 nombre2  -- [Llama a la funció gano con jug nombre1 nombre2]
                                imprimir "Presione Enter para volver al menú..."  --[Texto del menú]
                                hFlush stdout  -- [Ordena la memoria para mostrar el texto en pantalla y luego permitir el ingreso]*(1)
                                fin <- getLine  -- [Guardo el valor ingresado en fin]
                                system "cls"  -- [Función q limpia la pantalla]
                                imprimir "\n"  -- [Salto de línea]
                                imprimir "\n"  -- [Salto de línea]
                                main  -- [Vuelve al main]
                         2 -> do  -- [Si elegir es 2]
                                system "cls"  -- [Función q limpia la pantalla]
                                imprimir "El juego de Wythoff es un juego de resta matemática para dos jugadores, que se juega con dos pilas de fichas.\nLos jugadores se turnan para quitar las fichas de una o ambas pilas; al retirar fichas de ambas pilas, el número de fichas que se retiran de cada pila debe ser igual (excepto si se quiere retirar 0 asteríscos de cada fila).\nEl juego termina cuando una persona quita el último contador o contadores, ganando así.\n"  --[Instrucciones del juego]
                                imprimir "\n"  --[Salto de línea]
                                imprimir "Presione Enter para volver al menú..."  -- [Texto del menú]
                                hFlush stdout  -- [Ordena la memoria para mostrar el texto en pantalla y luego permitir el ingreso]*(1)
                                fin <- getLine  -- [Guarda en fin el valor ingresado]
                                system "cls"  -- [Función q limpia la pantalla]
                                imprimir "\n"  -- [Salto de línea]
                                imprimir "\n"  -- [Salto de línea]
                                main  -- [Vuelve al main]
                         3 -> do  -- [Si elegir es 3]
                                system "cls"  -- [Función q limpia la pantalla]
                                return True  -- [Devuelve true. Sale del programa]
                         otherwise -> do  -- [Caso por defecto]
                                        imprimir "No se seleccionó una opción válida"  -- [Texto del menú]
                                        return False  -- [Devuelve false. Sale del programa]

{- *(1) - GHC por default maneja memoria del buffer en orden de lìnea, por ende muestra en pantalla el texto a
          imprimir en orden de jerarquía.
-}