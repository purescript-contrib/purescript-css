module Css.String where

class IsString s where
  fromString :: String -> s

instance isStringString :: IsString String where
  fromString = id

