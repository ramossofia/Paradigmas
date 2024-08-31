module Anuncio (Anuncio, nuevoA, nombreA, duracionA, departamentosA, agregarA, sacarA, aplicaA) where

import Tipos

data Anuncio = Anu Nombre [Departamento] Duracion deriving (Eq, Show, Ord)

nuevoA :: Nombre -> Duracion -> Anuncio -- dado un nombre y una duracion en segundos retorna un nuevo Anuncio
nuevoA nombre duracion
  | duracion < 0 = error "La duración no puede ser negativa."
  | otherwise = Anu (verificarVacio nombre "No se ha ingresado un nombre.") [] duracion

nombreA :: Anuncio -> Nombre -- dado un anuncio retorna su nombre
nombreA (Anu nombre _ _) = verificarVacio nombre "No se encontró el anuncio."

duracionA :: Anuncio -> Duracion -- dado un anuncio retorna su duracion
duracionA (Anu _ _ duracion)
  | duracion <= 0 = error "El anuncio no tiene duración válida."
  | otherwise = duracion

verificarVacioDep :: [Departamento] -> String -> [Departamento] -- funcion auxiliar para verificar que la lista de departamentos no este vacia
verificarVacioDep [] mensajeError = error mensajeError
verificarVacioDep departamentos _ = departamentos

departamentosA :: Anuncio -> [Departamento] -- dado un anuncio retorna los departamentos que le fueron asociados
departamentosA (Anu _ departamentos _) = verificarVacioDep departamentos "No hay departamentos asociados con el anuncio."

agregarA :: Departamento -> Anuncio -> Anuncio -- permite asignar un departamento a un anuncio
agregarA departamento (Anu nombre departamentos duracion)
  | departamento `elem` departamentos = error "El departamento ya está asociado con el anuncio."
  | otherwise = Anu nombre (departamentos ++ [verificarVacio departamento "No se ha ingresado un nombre de departamento."]) duracion

sacarA :: Departamento -> Anuncio -> Anuncio -- permite quitarle un departamento a un anuncio
sacarA departamento (Anu nombre departamentos duracion)
  | null departamentos = error "El anuncio no tiene departamentos asociados."
  | not (departamento `elem` departamentos) = error "El departamento no existe en el anuncio."
  | otherwise = Anu nombre (filter (/= departamento) departamentos) duracion

aplicaA :: [Departamento] -> Anuncio -> Bool
aplicaA [] _ = error "La lista de departamentos proporcionada está vacía."
aplicaA deptos (Anu _ departamentos _) = any (`elem` departamentos) deptos
