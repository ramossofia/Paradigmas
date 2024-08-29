module FileSystem (FileSystem, buscarAnuncio ,nuevoF, departamentosF, anunciosF, agregarAnuncioF, sacarAnuncioF, agregarDepartamentoF, sacarDepartamentoF, anunciosParaF) where

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
agregarAnuncioF anuncio (FS departamentos anuncios)
    | anuncio `elem` anuncios = error "El anuncio ya se encuentra en el sistema."
    | otherwise = FS departamentos (anuncio : anuncios)

sacarAnuncioF :: Anuncio -> FileSystem -> FileSystem -- permite eliminar un anuncio
sacarAnuncioF anuncio (FS departamentos anuncios)
    | anuncio `notElem` anuncios = error "El anuncio no se encuentra en el sistema."
    | otherwise = FS departamentos (filter (/= anuncio) anuncios)

agregarDepartamentoF :: Departamento -> FileSystem -> FileSystem -- permite agregar un departamento
agregarDepartamentoF departamento (FS departamentos anuncios)
    | departamento `elem` departamentos = error "El departamento ya existe en el sistema."
    | otherwise = FS (departamento : departamentos) anuncios

sacarDepartamentoF :: Departamento -> FileSystem -> FileSystem -- permite eliminar un departamento
sacarDepartamentoF departamento (FS departamentos anuncios)
    | departamento `notElem` departamentos = error "El departamento no se encuentra en el sistema."
    | otherwise = FS (filter (/= departamento) departamentos) anuncios

anunciosParaF :: [Departamento] -> FileSystem -> [Anuncio]
anunciosParaF [] _ = error "No se proporcionaron departamentos."
anunciosParaF departamentos (FS _ anuncios) = filter (aplicaA departamentos) anuncios

    
buscarAnuncio :: Nombre -> FileSystem -> Anuncio --auxiliar
buscarAnuncio nombre fs =
  case filter ((== nombre) . nombreA) (anunciosF fs) of
    [anuncio] -> anuncio
    _         -> error "Advertencia: Anuncio no encontrado en el FileSystem"
