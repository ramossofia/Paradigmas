module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

anunciosConfigurados :: Prompter -> [Anuncio]
anunciosConfigurados (Pro fs deps _) = anunciosParaF deps fs

verificarIndice :: Int -> Int -> Int
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
anunciosP prompter@(Pro _ _ _)
  | null anuncios = []  -- Cambiado para retornar una lista vacía en lugar de lanzar una excepción
  | otherwise = map nombreA anuncios
  where
    anuncios = anunciosConfigurados prompter

showP :: Prompter -> Anuncio -- muestra el anuncio actual
showP prompter@(Pro _ _ index)
  | null anuncios = error "Error: No hay anuncios configurados."
  | otherwise = anuncios !! (index `mod` length anuncios)
  where
    anuncios = anunciosConfigurados prompter

avanzarP :: Prompter -> Prompter -- pasa al siguiente anuncio
avanzarP prompter@(Pro fs deps index)
  | null anuncios = error "Error: No hay anuncios configurados para avanzar."
  | otherwise = Pro fs deps nextIndex
  where
    anuncios = anunciosConfigurados prompter
    nextIndex = (index + 1) `mod` length anuncios

duracionP :: Prompter -> Duracion -- indica la duracion total de los anuncios configurados
duracionP prompter@(Pro _ _ _)
  | null anuncios = 0
  | otherwise = sum (map duracionA anuncios)
  where
    anuncios = anunciosConfigurados prompter
