module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

nuevoP :: FileSystem -> Prompter -- permite obtener un nuevo Prompter en base a un FileSystem
nuevoP fs = Pro fs [] 0

archivosR :: Prompter -> FileSystem -- dado un prompter retorna su fileSystem
archivosR (Pro fs _ _) = fs

departamentosP :: Prompter -> [Departamento] -- dado un prompter retorna sus departamentos
departamentosP (Pro _ deps _) = deps

configurarP :: Prompter -> [Departamento] -> Prompter -- Prepara el prompter para emitir los anuncios en los departementos indicados
configurarP (Pro fs _ index) deps = Pro fs deps index

anunciosP :: Prompter -> [Nombre] -- entrega la lista de nombres de anuncios configurados
anunciosP (Pro fs _ _) = map nombreA (anunciosF fs)

showP :: Prompter -> Anuncio -- muestra el anuncio actual
showP (Pro fs _ index) = (anunciosF fs) !! index

avanzarP :: Prompter -> Prompter -- pasa al siguiente anuncio
avanzarP (Pro fs deps index) = Pro fs deps (index + 1)

duracionP :: Prompter -> Duracion -- indica la duracion total de los anuncios configurados
duracionP (Pro fs _ _) = sum (map duracionA (anunciosParaF fs))
