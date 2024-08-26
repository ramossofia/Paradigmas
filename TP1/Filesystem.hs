module FileSystem (FileSystem, nuevoF, departamentosF, anunciosF, agregarAnuncioF, sacarAnuncioF, agregarDepartamentoF, sacarDepartamentoF, anunciosParaF) where

import Anuncio
import Tipos

data FileSystem = FS [Departamento] [Anuncio] deriving (Eq, Show)

nuevoF :: FileSystem -- permite obtener un nuevo FileSystem
nuevoF = FS [] []

departamentosF :: FileSystem -> [Departamento] -- dado un FileSystem retorna los departamentos que incluye
departamentosF (FS deps _) = deps

anunciosF :: FileSystem -> [Anuncio] -- dado un FileSystem retorna los anuncios que incluye
anunciosF (FS _ anuncios) = anuncios

agregarAnuncioF :: Anuncio -> FileSystem -> FileSystem -- permite agregar un anuncio
agregarAnuncioF anuncio (FS departamentos anuncios) = FS departamentos (anuncio : anuncios)

sacarAnuncioF :: Anuncio -> FileSystem -> FileSystem -- permite eliminar un anuncio
sacarAnuncioF anuncio (FS departamentos anuncios) = FS departamentos (filter (/= anuncio) anuncios)

agregarDepartamentoF :: Departamento -> FileSystem -> FileSystem -- permite agregar un departamento
agregarDepartamentoF departamento (FS departamentos anuncios) = FS (departamento : departamentos) anuncios

sacarDepartamentoF :: Departamento -> FileSystem -> FileSystem -- permite eliminar un departamento
sacarDepartamentoF departamento (FS departamentos anuncios) = FS (filter (/= departamento) departamentos) anuncios

anunciosParaF :: [Departamento] -> FileSystem -> [Anuncio] -- entrega los anuncios a emitir para un conjunto de departamentos
anunciosParaF departamentos (FS _ anuncios) = filter (aplicaA departamentos) anuncios
