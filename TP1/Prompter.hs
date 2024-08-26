module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

-- Función auxiliar: Permite obtener la lista de anuncios configurados
anunciosConfigurados :: Prompter -> [Anuncio]
anunciosConfigurados prompter = anunciosParaF (departamentosP prompter) (archivosR prompter)

nuevoP :: FileSystem -> Prompter -- permite obtener un nuevo Prompter en base a un FileSystem
nuevoP fs = Pro fs [] 0

archivosR :: Prompter -> FileSystem -- dado un prompter retorna su fileSystem
archivosR (Pro fs _ _) = fs

departamentosP :: Prompter -> [Departamento] -- dado un prompter retorna sus departamentos
departamentosP (Pro _ deps _) = deps

configurarP :: Prompter -> [Departamento] -> Prompter -- Prepara el prompter para emitir los anuncios en los departementos indicados
configurarP (Pro fs _ index) deps = Pro fs deps index

anunciosP :: Prompter -> [Nombre] -- entrega la lista de nombres de anuncios configurados
anunciosP prompter = map nombreA (anunciosConfigurados prompter)

showP :: Prompter -> Anuncio -- muestra el anuncio actual
showP prompter
  | null anuncios = error "No hay anuncios configurados"
  | otherwise = anuncios !! (index `mod` length anuncios)
  where
    anuncios = anunciosConfigurados prompter
    index = currentIndex prompter
    currentIndex (Pro _ _ idx) = idx

avanzarP :: Prompter -> Prompter -- pasa al siguiente anuncio
avanzarP prompter = Pro fs deps nextIndex
  where
    fs = archivosR prompter
    deps = departamentosP prompter
    anuncios = anunciosConfigurados prompter
    nextIndex
      | null anuncios = 0
      | otherwise = (index + 1) `mod` length anuncios
    index = currentIndex prompter
    currentIndex (Pro _ _ idx) = idx

duracionP :: Prompter -> Duracion -- indica la duracion total de los anuncios configurados
duracionP (Pro fs deps _) = foldr ((+) . duracionA) 0 (anunciosParaF deps fs)
