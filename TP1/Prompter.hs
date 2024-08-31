-- Prompter.hs
module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

anunciosConfigurados :: FileSystem -> [Departamento] -> [Anuncio]
anunciosConfigurados fs deps = anunciosParaF deps fs

verificarIndice :: Int -> Int -> Int
verificarIndice idx len
  | len == 0 = error "Error: No hay anuncios configurados."
  | idx < 0 || idx >= len = error "Error: Índice fuera de rango."
  | otherwise = idx

nuevoP :: FileSystem -> Prompter
nuevoP fs = Pro fs [] 0

archivosR :: Prompter -> FileSystem
archivosR (Pro fs _ _) = fs

departamentosP :: Prompter -> [Departamento]
departamentosP (Pro _ deps _) = deps

configurarP :: Prompter -> [Departamento] -> Prompter
configurarP (Pro fs _ index) deps
  | null depsValidos = error "Error: No se han configurado departamentos válidos."
  | otherwise = Pro fs depsValidos index
  where
    depsValidos = filter (`elem` departamentosF fs) deps

anunciosP :: Prompter -> [Nombre]
anunciosP (Pro fs deps _) =
  let anuncios = anunciosConfigurados fs deps
  in if null anuncios then [] else map nombreA anuncios

showP :: Prompter -> Anuncio
showP (Pro fs deps index) =
  let anuncios = anunciosConfigurados fs deps
  in if null anuncios then error "Error: No hay anuncios configurados."
     else anuncios !! (index `mod` length anuncios)

avanzarP :: Prompter -> Prompter
avanzarP (Pro fs deps index) =
  let anuncios = anunciosConfigurados fs deps
      nextIndex = (index + 1) `mod` length anuncios
  in if null anuncios then error "Error: No hay anuncios configurados para avanzar."
     else Pro fs deps nextIndex

duracionP :: Prompter -> Duracion
duracionP (Pro fs deps _) =
  let anuncios = anunciosConfigurados fs deps
  in foldl (\acc anuncio -> acc + duracionA anuncio) 0 anuncios
