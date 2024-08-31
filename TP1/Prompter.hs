module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

anunciosConfigurados :: Prompter -> [Anuncio] -- funcion auxiliar que retorna los anuncios configurados
anunciosConfigurados (Pro fs deps _) = anunciosParaF deps fs

verificarIndice :: Int -> Int -> Int -- funcion auxiliar que verifica si el indice es valido
verificarIndice idx len
  | len == 0 = error "Error: No hay anuncios configurados."
  | idx < 0 || idx >= len = error "Error: Índice fuera de rango."
  | otherwise = idx

nuevoP :: FileSystem -> Prompter -- permite obtener un nuevo Prompter en base a un FileSystem
nuevoP fs = Pro fs [] 0

archivosR :: Prompter -> FileSystem -- dado un prompter retorna su fileSystem
archivosR (Pro fs _ _) = fs

departamentosP :: Prompter -> [Departamento] -- dado un prompter retorna sus departamentos
departamentosP (Pro _ deps _) = deps

configurarP :: Prompter -> [Departamento] -> Prompter -- Prepara el prompter para emitir los anuncios en los departementos indicados
configurarP (Pro fs _ index) deps
  | null depsValidos = error "Error: No se han configurado departamentos válidos."
  | otherwise = Pro fs depsValidos index
  where
    depsValidos = filter (`elem` departamentosF fs) deps

anunciosP :: Prompter -> [Nombre] -- entrega la lista de nombres de anuncios configurados
anunciosP (Pro fs deps _)
  | null anuncios = []
  | otherwise = map nombreA anuncios
  where
    anuncios = anunciosParaF deps fs
showP :: Prompter -> Anuncio -- muestra el anuncio actual
showP (Pro fs deps index)
  | null anuncios = error "Error: No hay anuncios configurados."
  | otherwise = anuncios !! (index `mod` length anuncios)
  where
    anuncios = anunciosParaF deps fs

avanzarP :: Prompter -> Prompter -- pasa al siguiente anuncio
avanzarP (Pro fs deps index)
  | null anuncios = error "Error: No hay anuncios configurados para avanzar."
  | otherwise = Pro fs deps nextIndex
  where
    anuncios = anunciosParaF deps fs
    nextIndex = (index + 1) `mod` length anuncios

duracionP :: Prompter -> Duracion -- indica la duracion total de los anuncios configurados
duracionP (Pro fs deps _)
  | null anuncios = 0
  | otherwise = sum (map duracionA anuncios)
  where
    anuncios = anunciosParaF deps fs
