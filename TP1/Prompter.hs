module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

-- Funciones auxiliares:
obtenerAnuncios :: Prompter -> [Anuncio] -- permite obtener la lista de anuncios desde el Filesystem
obtenerAnuncios (Pro fs _ _) = anunciosF fs

anunciosConfigurados :: Prompter -> [Anuncio] -- permite obtener la lista de anuncios configurados
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
showP prompter = (anunciosConfigurados prompter) !! (index `mod` length anuncios)
  where
    anuncios = anunciosConfigurados prompter
    index = case anuncios of
      [] -> 0
      _ -> currentIndex prompter
    currentIndex (Pro _ _ idx) = idx

avanzarP :: Prompter -> Prompter -- pasa al siguiente anuncio
avanzarP prompter = Pro fs deps nextIndex
  where
    fs = archivosR prompter
    deps = departamentosP prompter
    anuncios = anunciosConfigurados prompter
    nextIndex = (currentIndex + 1) `mod` length anuncios
    currentIndex = case anuncios of
      [] -> 0
      _ -> index
    index = case prompter of
      Pro _ _ idx -> idx

duracionP :: Prompter -> Duracion -- indica la duracion total de los anuncios configurados
duracionP (Pro fs deps _) = foldr ((+) . duracionA) 0 (anunciosParaF deps fs)
