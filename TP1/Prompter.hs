module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

anunciosConfigurados :: Prompter -> [Anuncio] -- función auxiliar: Permite obtener la lista de anuncios configurados
anunciosConfigurados prompter = anunciosParaF (departamentosP prompter) (archivosR prompter)

verificarIndice :: Int -> Int -> Int -- función auxiliar: Verifica que el índice sea válido
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
anunciosP prompter
  | null anuncios = error "Advertencia: No hay anuncios configurados para los departamentos actuales."
  | otherwise = map nombreA anuncios
  where
    anuncios = anunciosConfigurados prompter

showP :: Prompter -> Anuncio -- muestra el anuncio actual
showP prompter
  | null anuncios = "No hay anuncios configurados"
  | otherwise = nombreA (anuncios !! (index `mod` length anuncios))
  where
    anuncios = anunciosConfigurados prompter
    index = verificarIndice (currentIndex prompter) (length anuncios)


avanzarP :: Prompter -> Prompter -- pasa al siguiente anuncio
avanzarP prompter
  | null anuncios = error "Error: No hay anuncios configurados para avanzar."
  | otherwise = Pro fs deps nextIndex
  where
    fs = archivosR prompter
    deps = departamentosP prompter
    anuncios = anunciosConfigurados prompter
    nextIndex = (currentIndex prompter + 1) `mod` length anuncios

duracionP :: Prompter -> Duracion -- indica la duracion total de los anuncios configurados
duracionP prompter
  | null anuncios = 0
  | otherwise = sum (map duracionA anuncios)
  where
    anuncios = anunciosConfigurados prompter
