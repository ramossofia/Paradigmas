module Prompter (Prompter, nuevoP, archivosR, departamentosP, configurarP, anunciosP, showP, avanzarP, duracionP) where

import Anuncio
import FileSystem
import Tipos

data Prompter = Pro FileSystem [Departamento] Int deriving (Eq, Show)

nuevoP :: FileSystem -> Prompter -- permite obtener un nuevo Prompter en base a un FileSystem
archivosR :: Prompter -> FileSystem -- dado un prompter retorna su fileSystem
departamentosP :: Prompter -> [Departamento] -- dado un prompter retorna sus departamentos
configurarP :: Prompter -> [Departamento] -> Prompter -- Prepara el prompter para emitir los anuncios en los departementos indicados
nunciosP :: Prompter -> [Nombre] -- entrega la lista de nombres de anuncios configurados
showP :: Prompter -> Anuncio -- muestra el anuncio actual
avanzarP :: Prompter -> Prompter -- pasa al siguiente anuncio
duracionP :: Prompter -> Duracion -- indica la duracion total de los anuncios configurados
