module Tipos where

type Duracion = Int

type Departamento = String

type Nombre = String

type Anuncio = (Nombre, [Departamento], Duracion)

type FileSystem = ([Departamento], [Anuncio])

type Prompter = ([Departamento], [Anuncio], [Departamento], Int)