module Tests where

import Anuncio
import Control.Exception
import FileSystem
import Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP)
import System.IO.Unsafe
import Tipos

-- Datos de ejemplo para pruebas
anuncio1 :: Anuncio
anuncio1 = nuevoA "Anuncio1" 30

anuncio2 :: Anuncio
anuncio2 = nuevoA "Anuncio2" 45

fileSystemEjemplo :: FileSystem
fileSystemEjemplo = agregarAnuncioF anuncio1 . agregarAnuncioF anuncio2 $ nuevoF

-- Función para probar excepciones
testF :: (Show a) => a -> Bool
testF action = unsafePerformIO $ do
  result <- tryJust isException (evaluate action)
  return $ case result of
    Left _ -> True
    Right _ -> False
  where
    isException :: SomeException -> Maybe ()
    isException _ = Just ()

-- Funciones de prueba para FileSystem

testNuevoF :: Bool
testNuevoF =
  departamentosF fs == [] && anunciosF fs == []
  where
    fs = nuevoF

testAgregarAnuncioF :: Bool
testAgregarAnuncioF =
  anunciosF fs == [anuncio1]
  where
    fs = agregarAnuncioF anuncio1 nuevoF

testSacarAnuncioF :: Bool
testSacarAnuncioF =
  anunciosF fs == [anuncio2]
  where
    fs = sacarAnuncioF anuncio1 fileSystemEjemplo

testAgregarDepartamentoF :: Bool
testAgregarDepartamentoF =
  departamentosF fs == ["Dept1"]
  where
    fs = agregarDepartamentoF "Dept1" nuevoF

testSacarDepartamentoF :: Bool
testSacarDepartamentoF =
  departamentosF fs == []
  where
    fs = sacarDepartamentoF "Dept1" (agregarDepartamentoF "Dept1" nuevoF)

testAnunciosParaF :: Bool
testAnunciosParaF =
  anuncios == [anuncio1, anuncio2]
  where
    fs = fileSystemEjemplo
    departamentos = ["Dept1"]
    anuncios = anunciosParaF departamentos fs


-- Funciones de prueba para Anuncio

testNuevoA :: Bool
testNuevoA =
  nombreA anuncio == "AnuncioTest" && duracionA anuncio == 60
  where
    anuncio = nuevoA "AnuncioTest" 60

testAgregarA :: Bool
testAgregarA =
  departamentosA anuncio == ["Dept1"]
  where
    anuncio = agregarA "Dept1" anuncio1

testSacarA :: Bool
testSacarA =
  departamentosA anuncio == []
  where
    anuncio = sacarA "Dept1" (agregarA "Dept1" anuncio1)

testAplicaA :: Bool
testAplicaA =
  aplicaA departamentos anuncioAplicable
  where
    anuncio = nuevoA "AnuncioTest" 60
    departamentos = ["Dept1"]
    anuncioAplicable = agregarA "Dept1" anuncio

-- Funciones de prueba para Prompter

testNuevoP :: Bool
testNuevoP =
  archivosIguales && departamentosIguales && indiceIgual
  where
    prompter = nuevoP fileSystemEjemplo
    archivosIguales = archivosR prompter == fileSystemEjemplo
    departamentosIguales = departamentosP prompter == []
    indiceIgual = currentIndex prompter == 0

    currentIndex :: Prompter -> Int
    currentIndex p = case showP p of
      "Anuncio1" -> 0
      _ -> 1


testConfigurarP :: Bool
testConfigurarP =
  departamentosPResult == ["Dept1", "Dept2"]
  where
    prompter = nuevoP fileSystemEjemplo
    -- Configura el Prompter con departamentos
    prompterConAnuncios = configurarP prompter ["Dept1", "Dept2"]
    departamentosPResult = departamentosP (configurarP prompterConAnuncios ["Dept1", "Dept2"])

testAnunciosP :: Bool
testAnunciosP =
  anunciosPResult == ["Anuncio1", "Anuncio2"]
  where
    prompter = nuevoP fileSystemEjemplo
    -- Configura el Prompter sin anuncios
    prompterConAnuncios = configurarP prompter []
    anunciosPResult = anunciosP (configurarP prompterConAnuncios ["Dept1"])

-- Prueba para showP
testShowP :: Bool
testShowP =
  showP prompterConfigurado == "No hay anuncios configurados"
  where
    prompter = nuevoP fileSystemEjemplo
    prompterConfigurado = configurarP prompter []

-- Prueba para avanzarP
testAvanzarP :: Bool
testAvanzarP =
  showP prompterAvanzado == "Anuncio2"
  where
    prompter = nuevoP fileSystemEjemplo
    prompterConfigurado = configurarP prompter ["Dept1"]
    prompterAvanzado = avanzarP prompterConfigurado

testDuracionP :: Bool
testDuracionP =
  duracionP prompterConfigurado == 75
  where
    prompter = nuevoP fileSystemEjemplo
    prompterConfigurado = configurarP prompter ["Dept1"]

-- Función para ejecutar todas las pruebas e imprimir los resultados
runTests :: IO ()
runTests = do
  let results = [ ("testNuevoF", testNuevoF)
                 , ("testAgregarAnuncioF", testAgregarAnuncioF)
                 , ("testSacarAnuncioF", testSacarAnuncioF)
                 , ("testAgregarDepartamentoF", testAgregarDepartamentoF)
                 , ("testSacarDepartamentoF", testSacarDepartamentoF)
                 , ("testAnunciosParaF", testAnunciosParaF)
                 , ("testNuevoA", testNuevoA)
                 , ("testAgregarA", testAgregarA)
                 , ("testSacarA", testSacarA)
                 , ("testAplicaA", testAplicaA)
                 , ("testNuevoP", testNuevoP)
                 , ("testConfigurarP", testConfigurarP)
                 , ("testAnunciosP", testAnunciosP)
                 , ("testShowP", testShowP)
                 , ("testAvanzarP", testAvanzarP)
                 , ("testDuracionP", testDuracionP)
                 ]
  let allPassed = all snd results
  mapM_ printResult results
  if allPassed
    then putStrLn "¡Todo OK!"
    else putStrLn "Algunas pruebas han fallado."

printResult :: (String, Bool) -> IO ()
printResult (name, result) =
  putStrLn $ name ++ ": " ++ if result then "Passed" else "Failed"

