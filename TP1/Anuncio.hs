module Anuncio (Anuncio, nuevoA, nombreA, duracionA, departamentosA, agregarA, sacarA, aplicaA) where

import Tipos

data Anuncio = Anu Nombre [Departamento] Duracion deriving (Eq, Show, Ord)

nuevoA :: Nombre -> Duracion -> Anuncio
nuevoA nombre duracion
    | duracion < 0 = error "La duración no puede ser negativa."
    | otherwise = Anu (verificarVacio nombre "No se ha ingresado un nombre.") [] duracion

nombreA :: Anuncio -> Nombre
nombreA (Anu nombre _ _) = verificarVacio nombre "No se encontró el anuncio."

duracionA :: Anuncio -> Duracion
duracionA (Anu _ _ duracion)
    | duracion <= 0 = error "El anuncio no tiene duración válida."
    | otherwise = duracion

verificarVacioDep :: [Departamento] -> String -> [Departamento]
verificarVacioDep [] mensajeError = error mensajeError
verificarVacioDep departamentos _ = departamentos

departamentosA :: Anuncio -> [Departamento]
departamentosA (Anu _ departamentos _) = verificarVacioDep departamentos "No hay departamentos asociados con el anuncio."

agregarA :: Departamento -> Anuncio -> Anuncio
agregarA departamento (Anu nombre departamentos duracion)
    | departamento `elem` departamentos = error "El departamento ya está asociado con el anuncio."
    | otherwise = Anu nombre (departamentos ++ [verificarVacio departamento "No se ha ingresado un nombre de departamento."]) duracion

sacarA :: Departamento -> Anuncio -> Anuncio
sacarA departamento (Anu nombre departamentos duracion)
    | null departamentos = error "El anuncio no tiene departamentos asociados."
    | not (departamento `elem` departamentos) = error "El departamento no existe en el anuncio."
    | otherwise = Anu nombre (filter (/= departamento) departamentos) duracion

aplicaA :: [Departamento] -> Anuncio -> Bool
aplicaA [] _ = error "Advertencia: Se ha pasado una lista vacía de departamentos."
aplicaA deps (Anu _ departamentos _) = any (`elem` departamentos) deps
