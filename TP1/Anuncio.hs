module Anuncio (Anuncio, nuevoA, nombreA, duracionA, departamentosA, agregarA, sacarA, aplicaA) where

import Tipos
 
data Anuncio = Anu Nombre [Departamento] Duracion deriving (Eq, Show, Ord)

--hacer auxiliar para chequear que ningun string escrito x terminal sea vacio.

nuevoA :: Nombre -> Duracion -> Anuncio -- dado un nombre y una duracion en segundos retorna un nuevo Anuncio
nuevoA nombre duracion = Anu nombre [] duracion
--casos de error: si se ingresa un nombre vacio que devuelva una excepcion y printee no se ha ingresado un nombre.
--si ya existe un anuncio con el nombre ingresado, que pida por terminal ingresar otro nombre.


nombreA :: Anuncio -> Nombre -- dado un anuncio retorna su nombre
nombreA (Anu nombre _ _) = nombre
--casos de error: que dado ese nombre no se halle ningun anuncio
-- que se ingrese un nombre vacio.


duracionA :: Anuncio -> Duracion -- dado un anuncio retorna su duracion
duracionA (Anu _ _ duracion) = duracion
--casos de error: no se halle el anuncio printee el error por terminal y tire excepcion
-- se envie un anuncio sin duracion y que la funcion devuelva 0

departamentosA :: Anuncio -> [Departamento] -- dado un anuncio retorna los departamentos que le fueron asociados
departamentosA (Anu _ departamentos _) = departamentos
--si el anuncio no tiene departamentos, que printee no hay departamentos asociados con el anuncio.


agregarA :: Departamento -> Anuncio -> Anuncio -- permite asignar un departamento a un anuncio
agregarA departamento (Anu nombre departamentos duracion) = Anu nombre (departamentos ++ [departamento]) duracion
--si ya existe un departamento con el nombre ingresado, printear que ya existe.


sacarA :: Departamento -> Anuncio -> Anuncio -- permite quitarle un departamento a un anuncio
sacarA departamento (Anu nombre departamentos duracion) = Anu nombre (filter (/= departamento) departamentos) duracion
--si no existe el anuncio ingresado, que printee que no existe.

aplicaA :: [Departamento] -> Anuncio -> Bool
aplicaA deptos (Anu _ departamentos _) = any (`elem` deptos) departamentos


