module Tests where

import Anuncio
import FileSystem (FileSystem, nuevoF, agregarAnuncioF, sacarAnuncioF, agregarDepartamentoF, sacarDepartamentoF, anunciosF, departamentosF, anunciosParaF, buscarAnuncio)
import Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP)
import Control.Exception
import System.IO.Unsafe
import Tipos

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

-- Pruebas para el módulo Anuncio (Excepciones)
testNuevoAConDuracionNegativa :: Bool
testNuevoAConDuracionNegativa = testF (nuevoA "Test" (-5))

testNombreAConNombreVacio :: Bool
testNombreAConNombreVacio =
  let anuncio = nuevoA "" 60
  in testF (nombreA anuncio)

testDuracionAConDuracionInvalida :: Bool
testDuracionAConDuracionInvalida =
  let anuncio = nuevoA "Test" 0
  in testF (duracionA anuncio)

testDepartamentosASinDepartamentos :: Bool
testDepartamentosASinDepartamentos = testF (departamentosA (nuevoA "Test" 60))

testAgregarAConDepartamentoRepetido :: Bool
testAgregarAConDepartamentoRepetido =
  let anuncio = agregarA "Dept1" (nuevoA "Test" 60)
  in testF (agregarA "Dept1" anuncio)

testSacarADeAnuncioSinDepartamentos :: Bool
testSacarADeAnuncioSinDepartamentos = testF (sacarA "Dept1" (nuevoA "Test" 60))

testAplicaAConDepartamentosVacios :: Bool
testAplicaAConDepartamentosVacios = testF (aplicaA [] (nuevoA "Test" 60))

-- Pruebas para el módulo FileSystem (Excepciones)
testAgregarAnuncioFRepetido :: Bool
testAgregarAnuncioFRepetido =
  let fs = agregarAnuncioF (nuevoA "Test" 60) nuevoF
  in testF (agregarAnuncioF (nuevoA "Test" 60) fs)

testSacarAnuncioFNoExistente :: Bool
testSacarAnuncioFNoExistente = testF (sacarAnuncioF (nuevoA "Test" 60) nuevoF)

testAgregarDepartamentoFRepetido :: Bool
testAgregarDepartamentoFRepetido =
  let fs = agregarDepartamentoF "Dept1" nuevoF
  in testF (agregarDepartamentoF "Dept1" fs)

testSacarDepartamentoFNoExistente :: Bool
testSacarDepartamentoFNoExistente = testF (sacarDepartamentoF "Dept1" nuevoF)

testAnunciosParaFConDepartamentosVacios :: Bool
testAnunciosParaFConDepartamentosVacios = testF (anunciosParaF [] nuevoF)

-- Funciones de prueba para Prompter (Excepciones)
testShowP :: Bool
testShowP =
  testF (showP prompterConfigurado)
  where
    prompter = nuevoP fileSystemEjemplo
    prompterConfigurado = configurarP prompter []

testAvanzarP :: Bool
testAvanzarP =
  testF (nombreA (showP prompterAvanzado) == "Anuncio2")
  where
    prompter = nuevoP fileSystemEjemplo
    prompterConfigurado = configurarP prompter ["Dept1"]
    prompterAvanzado = avanzarP prompterConfigurado

-- Pruebas de funcionamiento normal (sin excepciones)

-- Ejemplo de datos para pruebas
anuncio1 :: Anuncio
anuncio1 = nuevoA "Anuncio1" 30

anuncio2 :: Anuncio
anuncio2 = nuevoA "Anuncio2" 45

fileSystemEjemplo :: FileSystem
fileSystemEjemplo = agregarAnuncioF anuncio1 . agregarAnuncioF anuncio2 $ nuevoF

-- Funciones de prueba para FileSystem (funcionamiento normal)
testNuevoF :: Bool
testNuevoF = departamentosF fs == [] && anunciosF fs == []
  where
    fs = nuevoF

testAgregarAnuncioF :: Bool
testAgregarAnuncioF = anunciosF fs == [anuncio1]
  where
    fs = agregarAnuncioF anuncio1 nuevoF

testSacarAnuncioF :: Bool
testSacarAnuncioF = anunciosF fs == [anuncio2]
  where
    fs = sacarAnuncioF anuncio1 fileSystemEjemplo

testAgregarDepartamentoF :: Bool
testAgregarDepartamentoF = departamentosF fs == ["Dept1"]
  where
    fs = agregarDepartamentoF "Dept1" nuevoF

testSacarDepartamentoF :: Bool
testSacarDepartamentoF = departamentosF fs == []
  where
    fs = sacarDepartamentoF "Dept1" (agregarDepartamentoF "Dept1" nuevoF)

testAnunciosParaF :: Bool
testAnunciosParaF = anuncios == [anuncio1, anuncio2]
  where
    fs = fileSystemEjemplo
    departamentos = ["Dept1"]
    anuncios = anunciosParaF departamentos fs

-- Funciones de prueba para Anuncio (funcionamiento normal)
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

testAplicaA :: Bool
testAplicaA =
  aplicaA departamentos anuncioAplicable
  where
    anuncio = nuevoA "AnuncioTest" 60
    departamentos = ["Dept1"]
    anuncioAplicable = agregarA "Dept1" anuncio

-- Funciones de prueba para Prompter (funcionamiento normal)
testConfigurarP :: Bool
testConfigurarP = departamentosPResult == ["Dept1", "Dept2"]
  where
    fsConDept1Dept2 = agregarDepartamentoF "Dept1" $ agregarDepartamentoF "Dept2" nuevoF
    prompter = nuevoP fsConDept1Dept2
    prompterConAnuncios = configurarP prompter ["Dept1", "Dept2"]
    departamentosPResult = departamentosP prompterConAnuncios



testAnunciosP :: Bool
testAnunciosP = anunciosPResult == ["Anuncio1", "Anuncio2"]
  where
    fsConDept1 = agregarDepartamentoF "Dept1" $ nuevoF
    fsConAnunciosYDeptos = agregarAnuncioF anuncio1 $ agregarAnuncioF anuncio2 fsConDept1
    prompter = nuevoP fsConAnunciosYDeptos
    prompterConAnuncios = configurarP prompter ["Dept1"]
    anunciosPResult = anunciosP prompterConAnuncios

testDuracionP :: Bool
testDuracionP = duracionP prompterConfigurado == 75
  where
    -- Inicializa el FileSystem con un departamento y dos anuncios
    fsConDept1 = agregarDepartamentoF "Dept1" $ nuevoF
    fsConAnunciosYDeptos = agregarAnuncioF anuncio1 $ agregarAnuncioF anuncio2 fsConDept1
    -- Inicializa el Prompter con el FileSystem configurado
    prompter = nuevoP fsConAnunciosYDeptos
    -- Configura el Prompter con el departamento y obtiene los anuncios configurados
    prompterConfigurado = configurarP prompter ["Dept1"]
    -- Obtiene los nombres de los anuncios configurados
    nombresAnuncios = anunciosP prompterConfigurado
    -- Busca los anuncios en el FileSystem
    anunciosEncontrados = map (\nombre -> buscarAnuncio nombre fsConAnunciosYDeptos) nombresAnuncios
    -- Calcula la duración esperada
    duracionEsperada = sum (map duracionA anunciosEncontrados)


-- Función para ejecutar todas las pruebas e imprimir los resultados
runTests :: IO ()
runTests = do
  putStrLn "\n#####################################"
  putStrLn "\nEjecutando pruebas de excepciones..."

  let exceptionResults = [ ("\ntestNuevoAConDuracionNegativa", testNuevoAConDuracionNegativa)
                         , ("testNombreAConNombreVacio", testNombreAConNombreVacio)
                         , ("testDuracionAConDuracionInvalida", testDuracionAConDuracionInvalida)
                         , ("testDepartamentosASinDepartamentos", testDepartamentosASinDepartamentos)
                         , ("testAgregarAConDepartamentoRepetido", testAgregarAConDepartamentoRepetido)
                         , ("testSacarADeAnuncioSinDepartamentos", testSacarADeAnuncioSinDepartamentos)
                         , ("testAplicaAConDepartamentosVacios", testAplicaAConDepartamentosVacios)
                         , ("testAgregarAnuncioFRepetido", testAgregarAnuncioFRepetido)
                         , ("testSacarAnuncioFNoExistente", testSacarAnuncioFNoExistente)
                         , ("testAgregarDepartamentoFRepetido", testAgregarDepartamentoFRepetido)
                         , ("testSacarDepartamentoFNoExistente", testSacarDepartamentoFNoExistente)
                         , ("testAnunciosParaFConDepartamentosVacios", testAnunciosParaFConDepartamentosVacios)
                         , ("testShowP", testShowP)
                         , ("testAvanzarP", testAvanzarP)
                         ]
  mapM_ printResult exceptionResults

  putStrLn "\n##############################################"
  putStrLn "\nEjecutando pruebas de funcionamiento normal..."
  let normalResults = [ ("testNuevoF", testNuevoF)
                      , ("testAgregarAnuncioF", testAgregarAnuncioF)
                      , ("testSacarAnuncioF", testSacarAnuncioF)
                      , ("testAgregarDepartamentoF", testAgregarDepartamentoF)
                      , ("testSacarDepartamentoF", testSacarDepartamentoF)
                      , ("testAnunciosParaF", testAnunciosParaF)
                      , ("testNuevoA", testNuevoA)
                      , ("testAgregarA", testAgregarA)
                      , ("testAplicaA", testAplicaA)
                      , ("testConfigurarP", testConfigurarP)
                      , ("testAnunciosP", testAnunciosP)
                      , ("testDuracionP", testDuracionP)
                      ]
  mapM_ printResult normalResults

  where
    printResult (testName, result) = putStrLn $ testName ++ ": " ++ (if result then "Passed" else "Failed")
