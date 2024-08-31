module Tests where

import Anuncio
import Control.Exception
import FileSystem (FileSystem, agregarAnuncioF, agregarDepartamentoF, anunciosF, anunciosParaF, departamentosF, nuevoF, sacarAnuncioF, sacarDepartamentoF)
import Prompter (Prompter, anunciosP, archivosR, avanzarP, configurarP, departamentosP, duracionP, nuevoP, showP)
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
testNombreAConNombreVacio = testF (nombreA (nuevoA "" 60))

testDuracionAConDuracionInvalida :: Bool
testDuracionAConDuracionInvalida = testF (duracionA (nuevoA "Test" 0))

testDepartamentosASinDepartamentos :: Bool
testDepartamentosASinDepartamentos = testF (departamentosA (nuevoA "Test" 60))

testAgregarAConDepartamentoRepetido :: Bool
testAgregarAConDepartamentoRepetido = testF (agregarA "Dept1" (agregarA "Dept1" (nuevoA "Test" 60)))

testSacarADeAnuncioSinDepartamentos :: Bool
testSacarADeAnuncioSinDepartamentos = testF (sacarA "Dept1" (nuevoA "Test" 60))

testAplicaAConDepartamentosVacios :: Bool
testAplicaAConDepartamentosVacios = testF (aplicaA [] (nuevoA "Test" 60))

-- Pruebas para el módulo FileSystem (Excepciones)
testAgregarAnuncioFRepetido :: Bool
testAgregarAnuncioFRepetido = testF (agregarAnuncioF (nuevoA "Test" 60) (agregarAnuncioF (nuevoA "Test" 60) nuevoF))

testSacarAnuncioFNoExistente :: Bool
testSacarAnuncioFNoExistente = testF (sacarAnuncioF (nuevoA "Test" 60) nuevoF)

testAgregarDepartamentoFRepetido :: Bool
testAgregarDepartamentoFRepetido = testF (agregarDepartamentoF "Dept1" (agregarDepartamentoF "Dept1" nuevoF))

testSacarDepartamentoFNoExistente :: Bool
testSacarDepartamentoFNoExistente = testF (sacarDepartamentoF "Dept1" nuevoF)

testAnunciosParaFConDepartamentosVacios :: Bool
testAnunciosParaFConDepartamentosVacios = testF (anunciosParaF [] nuevoF)

-- Funciones de prueba para Prompter (Excepciones)
testShowP :: Bool
testShowP = testF (showP (configurarP (nuevoP fileSystemEjemplo) []))

testAvanzarP :: Bool
testAvanzarP = testF (nombreA (showP (avanzarP (configurarP (nuevoP fileSystemEjemplo) ["Dept1"]))) == "Anuncio2")

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
testNuevoF = departamentosF nuevoF == [] && anunciosF nuevoF == []

testAgregarAnuncioF :: Bool
testAgregarAnuncioF = anunciosF (agregarAnuncioF anuncio1 nuevoF) == [anuncio1]

testSacarAnuncioF :: Bool
testSacarAnuncioF = anunciosF (sacarAnuncioF anuncio1 fileSystemEjemplo) == [anuncio2]

testAgregarDepartamentoF :: Bool
testAgregarDepartamentoF = departamentosF (agregarDepartamentoF "Dept1" nuevoF) == ["Dept1"]

testSacarDepartamentoF :: Bool
testSacarDepartamentoF = departamentosF (sacarDepartamentoF "Dept1" (agregarDepartamentoF "Dept1" nuevoF)) == []

testAnunciosParaF :: Bool
testAnunciosParaF =
  anunciosParaF ["nuevoDepto"] (agregarDepartamentoF "nuevoDepto" fileSystemEjemplo) == []

-- Funciones de prueba para Anuncio (funcionamiento normal)
testNuevoA :: Bool
testNuevoA =
  nombreA (nuevoA "AnuncioTest" 60) == "AnuncioTest" && duracionA (nuevoA "AnuncioTest" 60) == 60

testAgregarA :: Bool
testAgregarA =
  departamentosA (agregarA "Dept1" anuncio1) == ["Dept1"]

testAplicaA :: Bool
testAplicaA =
  aplicaA ["Dept1"] (agregarA "Dept1" (nuevoA "AnuncioTest" 60))

-- Funciones de prueba para Prompter (funcionamiento normal)
testConfigurarP :: Bool
testConfigurarP =
  departamentosP (configurarP (nuevoP (agregarDepartamentoF "Dept1" (agregarDepartamentoF "Dept2" nuevoF))) ["Dept1", "Dept2"]) == ["Dept1", "Dept2"]

testAnunciosP :: Bool
testAnunciosP =
  anunciosP (configurarP (nuevoP (agregarAnuncioF anuncio1 (agregarAnuncioF anuncio2 (agregarDepartamentoF "Dept1" nuevoF)))) ["Dept1"]) == ["Anuncio1", "Anuncio2"]

testDuracionP :: Bool
testDuracionP =
  sum (map duracionA (anunciosParaF ["Dept1"] (agregarAnuncioF anuncio1 (agregarAnuncioF anuncio2 (agregarDepartamentoF "Dept1" nuevoF))))) == 75

-- Función para ejecutar todas las pruebas e imprimir los resultados
runTests :: IO ()
runTests = do
  putStrLn "\n#####################################"
  putStrLn "\nEjecutando pruebas de excepciones..."

  let exceptionResults =
        [ ("testNuevoAConDuracionNegativa", testNuevoAConDuracionNegativa),
          ("testNombreAConNombreVacio", testNombreAConNombreVacio),
          ("testDuracionAConDuracionInvalida", testDuracionAConDuracionInvalida),
          ("testDepartamentosASinDepartamentos", testDepartamentosASinDepartamentos),
          ("testAgregarAConDepartamentoRepetido", testAgregarAConDepartamentoRepetido),
          ("testSacarADeAnuncioSinDepartamentos", testSacarADeAnuncioSinDepartamentos),
          ("testAplicaAConDepartamentosVacios", testAplicaAConDepartamentosVacios),
          ("testAgregarAnuncioFRepetido", testAgregarAnuncioFRepetido),
          ("testSacarAnuncioFNoExistente", testSacarAnuncioFNoExistente),
          ("testAgregarDepartamentoFRepetido", testAgregarDepartamentoFRepetido),
          ("testSacarDepartamentoFNoExistente", testSacarDepartamentoFNoExistente),
          ("testAnunciosParaFConDepartamentosVacios", testAnunciosParaFConDepartamentosVacios),
          ("testShowP", testShowP),
          ("testAvanzarP", testAvanzarP)
        ]
  mapM_ printResult exceptionResults

  putStrLn "\n##############################################"
  putStrLn "\nEjecutando pruebas de funcionamiento normal..."
  let normalResults =
        [ ("testNuevoF", testNuevoF),
          ("testAgregarAnuncioF", testAgregarAnuncioF),
          ("testSacarAnuncioF", testSacarAnuncioF),
          ("testAgregarDepartamentoF", testAgregarDepartamentoF),
          ("testSacarDepartamentoF", testSacarDepartamentoF),
          ("testAnunciosParaF", testAnunciosParaF),
          ("testNuevoA", testNuevoA),
          ("testAgregarA", testAgregarA),
          ("testAplicaA", testAplicaA),
          ("testConfigurarP", testConfigurarP),
          ("testAnunciosP", testAnunciosP),
          ("testDuracionP", testDuracionP)
        ]
  mapM_ printResult normalResults
  where
    printResult (testName, result) = putStrLn $ testName ++ ": " ++ (if result then "Passed" else "Failed")
