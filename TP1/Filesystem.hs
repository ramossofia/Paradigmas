module FileSystem (FileSystem, nuevoF, departamentosF, anunciosF, agregarAnuncioF, sacarAnuncioF, agregarDepartamentoF, sacarDepartamentoF, anunciosParaF) where

import Anuncio
import Tipos

data FileSystem = FS [Departamento] [Anuncio] deriving (Eq, Show)

nuevoF :: FileSystem
nuevoF = FS [] []

departamentosF :: FileSystem -> [Departamento]
departamentosF (FS deps _) = deps
--si no hay departamentos que devuelva una lista vacia


anunciosF :: FileSystem -> [Anuncio]
anunciosF (FS _ anuncios) = anuncios
--si no hay anuncios que devuelva una lista vacia


agregarAnuncioF :: Anuncio -> FileSystem -> FileSystem
agregarAnuncioF anuncio (FS departamentos anuncios) = FS departamentos (anuncio : anuncios)
--si ya existe el anuncio printear que el anuncio se encuentra en sistema y no agregar nada.

sacarAnuncioF :: Anuncio -> FileSystem -> FileSystem
sacarAnuncioF anuncio (FS departamentos anuncios) = FS departamentos (filter (/= anuncio) anuncios)
-- si no se encuentra el anuncio  en el sistema de archivos printear que no se encuentra ese archivo.

agregarDepartamentoF :: Departamento -> FileSystem -> FileSystem
agregarDepartamentoF departamento (FS departamentos anuncios) = FS (departamento : departamentos) anuncios
--si ya existe el departamento en el sistema de archivos que no lo agregue de nuevo y printee que ya existe .

sacarDepartamentoF :: Departamento -> FileSystem -> FileSystem
sacarDepartamentoF departamento (FS departamentos anuncios) = FS (filter (/= departamento) departamentos) anuncios
--si nos e encuentra el departamento en el sistema de archivos printear que no se encuentra.

anunciosParaF :: [Departamento] -> FileSystem -> [Anuncio]
anunciosParaF departamentos (FS _ anuncios) = filter (\a -> aplicaA departamentos a) anuncios
--si9 se ingresa un departamento que no existe, devolver vacio.
