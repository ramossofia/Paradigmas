module Tests where

import Anuncio
import FileSystem
import Prompter
import Tipos

-- Ejemplos de datos para pruebas
anuncio1 :: Anuncio
anuncio1 = nuevoA "Anuncio1" 30

anuncio2 :: Anuncio
anuncio2 = nuevoA "Anuncio2" 45

fileSystemEjemplo :: FileSystem
fileSystemEjemplo = agregarAnuncioF anuncio1 . agregarAnuncioF anuncio2 $ nuevoF

-- Función de prueba para nuevoP
testNuevoP :: IO ()
testNuevoP = do
  let fs = fileSystemEjemplo
  let prompter = nuevoP fs
  let currentIndex = 0 -- Simulate the currentIndex value
  putStrLn "Prueba nuevoP:"
  if archivosR prompter == fs && departamentosP prompter == [] && currentIndex == 0
    then putStrLn "  Pasó."
    else putStrLn "  Falló."

-- Función de prueba para configurarP
testConfigurarP :: IO ()
testConfigurarP = do
  let fs = fileSystemEjemplo
  let prompter = nuevoP fs
  let prompterConfigurado = configurarP prompter ["Dept1", "Dept2"]
  putStrLn "Prueba configurarP:"
  if departamentosP prompterConfigurado == ["Dept1", "Dept2"]
    then putStrLn "  Pasó."
    else putStrLn "  Falló."

-- Función de prueba para anunciosP
testAnunciosP :: IO ()
testAnunciosP = do
  let fs = fileSystemEjemplo
  let prompter = nuevoP fs
  let prompterConfigurado = configurarP prompter []
  putStrLn "Prueba anunciosP:"
  if anunciosP prompterConfigurado == ["Anuncio1", "Anuncio2"]
    then putStrLn "  Pasó."
    else putStrLn "  Falló."

-- Función de prueba para showP
testShowP :: IO ()
testShowP = do
  let fs = fileSystemEjemplo
  let prompter = nuevoP fs
  let prompterConfigurado = configurarP prompter []
  putStrLn "Prueba showP:"
  if nombreA (showP prompterConfigurado) == "Anuncio1"
    then putStrLn "  Pasó."
    else putStrLn "  Falló."

-- Función de prueba para avanzarP
testAvanzarP :: IO ()
testAvanzarP = do
  let fs = fileSystemEjemplo
  let prompter = nuevoP fs
  let prompterConfigurado = configurarP prompter []
  let prompterAvanzado = avanzarP prompterConfigurado
  putStrLn "Prueba avanzarP:"
  if nombreA (showP prompterAvanzado) == "Anuncio2"
    then putStrLn "  Pasó."
    else putStrLn "  Falló."

-- Función de prueba para duracionP
testDuracionP :: IO ()
testDuracionP = do
  let fs = fileSystemEjemplo
  let prompter = nuevoP fs
  let prompterConfigurado = configurarP prompter []
  putStrLn "Prueba duracionP:"
  if duracionP prompterConfigurado == 75
    then putStrLn "  Pasó."
    else putStrLn "  Falló."

-- Ejecución de todas las pruebas
runTests :: IO ()
runTests = do
  testNuevoP
  testConfigurarP
  testAnunciosP
  testShowP
  testAvanzarP
  testDuracionP
