module Anuncio (Anuncio, nuevoA, nombreA, duracionA, departamentosA, agregarA, sacarA, aplicaA) where

import Tipos

data Anuncio = Anu Nombre [Departamento] Duracion deriving (Eq, Show, Ord)

nuevoA :: Nombre -> Duracion -> Anuncio -- dado un nombre y una duracion en segundos retorna un nuevo Anuncio
nuevoA nombre duracion = Anu nombre [] duracion

nombreA :: Anuncio -> Nombre -- dado un anuncio retorna su nombre
nombreA (Anu nombre _ _) = nombre

duracionA :: Anuncio -> Duracion -- dado un anuncio retorna su duracion
duracionA (Anu _ _ duracion) = duracion

departamentosA :: Anuncio -> [Departamento] -- dado un anuncio retorna los departamentos que le fueron asociados
departamentosA (Anu _ departamentos _) = departamentos

agregarA :: Departamento -> Anuncio -> Anuncio -- permite asignar un departamento a un anuncio
agregarA departamento (Anu nombre departamentos duracion) = Anu nombre (departamentos ++ [departamento]) duracion

sacarA :: Departamento -> Anuncio -> Anuncio -- permite quitarle un departamento a un anuncio
sacarA departamento (Anu nombre departamentos duracion) = Anu nombre (filter (/= departamento) departamentos) duracion

aplicaA :: [Departamento] -> Anuncio -> Bool -- responde si un anuncio debe emitirse para alguno de los departamentos consultados
aplicaA deptos (Anu _ departamentos _) = any (`elem` departamentos) deptos
