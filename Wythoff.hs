import System.IO ( IO, hFlush, stdout, getLine, putChar, putStrLn )
import Prelude
import System.Random ( randomRIO )

inicio = do
            putStrLn "¿Qué desea hacer?"
            putStrLn "(1) Jugar una partida"
            putStrLn "(2) Ver instrucciones"
            putStrLn "(3) Salir del juego"
            imprimir "\n"
            imprimir "Selecciona una opción: "
            hFlush stdout
            eleccion <- getLine
            imprimir "\n"
            let elec = read eleccion :: Int
            if elec <= 0 || elec > 3 then putStrLn ("Numero invalido" ++ "\n") else imprimir ""
            if elec <= 0 || elec > 3 then inicio else do return elec
            return elec


ingresoMax :: IO Int
ingresoMax = do putStrLn "Jugador 1"
                imprimir "Elija el numero maximo por fila: "
                hFlush stdout
                maximo <- getLine
                imprimir "\n"
                let max = read maximo :: Int
                if max <= 1  then putStrLn ("Numero invalido" ++ "\n") else imprimir ""
                if max <= 1 then ingresoMax else do return max


imprimir [ ] = return ()
imprimir (x : xs) = do
                        putChar x
                        imprimir xs


dif x xs z =  do
    y <- randomRIO(1, z)
    if x == xs then dif x y z else return (x, xs)

dibujarFila x =do if x /= 0 then "*"++dibujarFila (x-1) else []

gano jug = if jug then putStrLn "Gano el jugador 1" else putStrLn "Gano el jugador 2"

jugar fila1 fila2 jug = do
                            let lista1 = dibujarFila fila1
                            let lista2 = dibujarFila fila2
                            putStrLn ("1 : " ++ lista1)
                            putStrLn ("2 : " ++ lista2 ++ "\n")
                            if jug then putStrLn "Jugador 1" else putStrLn "Jugador 2"
                            imprimir "de fila 1 saco: "
                            hFlush stdout
                            i1 <- getLine
                            let input1 = read i1 :: Int
                            if fila1 < input1 || input1 < 0 then putStrLn ("Jugada invalida" ++ "\n") else imprimir ""
                            if fila1 < input1 || input1 < 0 then jugar fila1 fila2 (not jug) else do
                                                                                        imprimir "de fila 2 saco: "
                                                                                        hFlush stdout
                                                                                        i2 <- getLine
                                                                                        let input2 = read i2 :: Int
                                                                                        if fila2 < input2 || input2 < 0 then putStrLn ("Jugada invalida" ++ "\n") else imprimir ""
                                                                                        if fila2 < input2 || input2 < 0 then jugar fila1 fila2 (not jug) else do
                                                                                                                                                    if ((input1 == input2) && (input1 == 0)) then putStrLn ("Movimiento inválido, no se puede saltar el turno"++"\n") else imprimir ""
                                                                                                                                                    if ((input1 == input2) && (input1 == 0)) then jugar fila1 fila2 jug else do 
                                                                                                                                                                                                                                        if ((input1 /= input2) && (input1 /= 0)) && (input2 /=0) then putStrLn ("No se puede eliminar distintas cantidades de las filas"++"\n") else imprimir ""
                                                                                                                                                                                                                                        if ((input1 /= input2) && (input1 /= 0)) && (input2 /=0) then jugar fila1 fila2 (not jug) else do
                                                                                                                                                                                                                                                                                                                                            let fila_1 = fila1 - input1
                                                                                                                                                                                                                                                                                                                                            let fila_2 = fila2 - input2
                                                                                                                                                                                                                                                                                                                                            if fila_1 == fila_2 && fila_1 == 0 then return jug else jugar fila_1 fila_2 (not jug)



main = do  
          putStrLn "BIENVENIDO A WYTHOFF"
          elegir <- inicio
          case elegir of 1 -> do 
                                xs <- ingresoMax
                                fila1 <- randomRIO(1, xs) :: IO Int
                                fila2 <- randomRIO(1, xs) :: IO Int
                                (y, x) <- dif fila1 fila2 xs
                                let fila1 = y
                                let fila2 = x
                                jug <- jugar fila1 fila2 True
                                gano jug
                                imprimir "Presione Enter para volver al menú..."
                                hFlush stdout
                                fin <- getLine
                                imprimir "\n"
                                imprimir "\n"
                                main
                         2 -> do
                                imprimir "El juego de Wythoff es un juego de resta matemática para dos jugadores, que se juega con dos pilas de fichas.\nLos jugadores se turnan para quitar las fichas de una o ambas pilas; al retirar fichas de ambas pilas, el número de fichas que se retiran de cada pila debe ser igual (excepto si se quiere retirar 0 asteríscos de cada fila).\nEl juego termina cuando una persona quita el último contador o contadores, ganando así.\n"
                                imprimir "\n"
                                imprimir "Presione Enter para volver al menú..."
                                hFlush stdout
                                fin <- getLine
                                imprimir "\n"
                                imprimir "\n"
                                main
                         3 -> return True
