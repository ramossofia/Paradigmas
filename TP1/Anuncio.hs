module Anuncio (Anuncio, nuevoA, nombreA, duracionA, departamentosA, agregarA, sacarA, aplicaA) where

import qualified Tipos as T 

data Anuncio = Anu T.Nombre [T.Departamento] T.Duracion deriving (Eq, Show, Ord)


nuevoA :: T.Nombre -> T.Duracion -> Anuncio -- dado un nombre y una duracion en segundos retorna un nuevo Anuncio
nuevoA nombre duracion = Anu nombre [] duracion


nombreA :: Anuncio -> T.Nombre -- dado un anuncio retorna su nombre
nombreA (Anu nombre _ _) = nombre


duracionA :: Anuncio -> T.Duracion -- dado un anuncio retorna su duracion
duracionA (Anu _ _ duracion) = duracion


departamentosA :: Anuncio -> [T.Departamento] -- dado un anuncio retorna los departamentos que le fueron asociados
departamentosA (Anu _ departamentos _) = departamentos


agregarA :: T.Departamento -> Anuncio -> Anuncio -- permite asignar un departamento a un anuncio
agregarA departamento (Anu nombre departamentos duracion) = Anu nombre (departamentos ++ [departamento]) duracion


sacarA :: T.Departamento -> Anuncio -> Anuncio -- permite quitarle un departamento a un anuncio
sacarA departamento (Anu nombre departamentos duracion) = Anu nombre (filter (/= departamento) departamentos) duracion


aplicaA :: [T.Departamento] -> Anuncio -> Bool -- responde si un anuncio debe emitirse para alguno de los departamentos consultados
aplicaA deptos (Anu _ departamentos _) = any (`elem` departamentos) deptos

