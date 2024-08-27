module Tipos where

type Duracion = Int

type Departamento = String

type Nombre = String

verificarVacio :: String -> String -> String
verificarVacio "" _ = error "El campo no puede estar vacio"
verificarVacio valor _ = valor
