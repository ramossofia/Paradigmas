module FileSystem (FileSystem, buscarAnuncio, nuevoF, departamentosF, anunciosF, agregarAnuncioF, sacarAnuncioF, agregarDepartamentoF, sacarDepartamentoF, anunciosParaF) where

import Anuncio
import Tipos

data FileSystem = FS [Departamento] [Anuncio] deriving (Eq, Show)

nuevoF :: FileSystem
nuevoF = FS [] []

departamentosF :: FileSystem -> [Departamento]
departamentosF (FS deps _) = deps

anunciosF :: FileSystem -> [Anuncio]
anunciosF (FS _ anuncios) = anuncios

agregarAnuncioF :: Anuncio -> FileSystem -> FileSystem
agregarAnuncioF anuncio (FS departamentos anuncios)
    | anuncio `elem` anuncios = error "El anuncio ya se encuentra en el sistema."
    | otherwise = FS departamentos (anuncio : anuncios)

sacarAnuncioF :: Anuncio -> FileSystem -> FileSystem
sacarAnuncioF anuncio (FS departamentos anuncios)
    | anuncio `notElem` anuncios = error "El anuncio no se encuentra en el sistema."
    | otherwise = FS departamentos (filter (/= anuncio) anuncios)

agregarDepartamentoF :: Departamento -> FileSystem -> FileSystem
agregarDepartamentoF departamento (FS departamentos anuncios)
    | departamento `elem` departamentos = error "El departamento ya existe en el sistema."
    | otherwise = FS (departamento : departamentos) anuncios

sacarDepartamentoF :: Departamento -> FileSystem -> FileSystem
sacarDepartamentoF departamento (FS departamentos anuncios)
    | departamento `notElem` departamentos = error "El departamento no se encuentra en el sistema."
    | otherwise = FS (filter (/= departamento) departamentos) anuncios

anunciosParaF :: [Departamento] -> FileSystem -> [Anuncio]
anunciosParaF [] _ = error "No se proporcionaron departamentos."
anunciosParaF deps (FS _ anuncios) = foldr (\anuncio acc -> if aplicaA deps anuncio then anuncio : acc else acc) [] anuncios

buscarAnuncio :: Nombre -> FileSystem -> Anuncio
buscarAnuncio nombre fs =
  case filter ((== nombre) . nombreA) (anunciosF fs) of
    [anuncio] -> anuncio
    _         -> error "Advertencia: Anuncio no encontrado en el FileSystem"
