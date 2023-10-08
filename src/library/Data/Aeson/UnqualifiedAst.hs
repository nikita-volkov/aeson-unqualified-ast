-- | Definitions of JSON AST in terms of the \"aeson\" package.
--
-- This module is intended to be imported unqualified.
-- It is obliged by the package policy to never pollute the namespace.
module Data.Aeson.UnqualifiedAst where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding (scientific)
import Data.Aeson.KeyMap qualified as Aeson
import Data.Bool (Bool)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

-- |
-- Alias to JSON AST representation in terms of the \"aeson\" package.
--
-- Together with the associated pattern synonym definitions that are defined in this module
-- this type provides virtually the same API as the following definition:
--
-- > data Json
-- >   = ObjectJson (KeyMap Json)
-- >   | ArrayJson (Vector Json)
-- >   | StringJson Text
-- >   | NumberJson Scientific
-- >   | BoolJson Bool
-- >   | NullJson
--
-- At the same time being just an alias it is completely interchangeable with the 'Aeson.Value' definition from \"aeson\" at zero runtime cost.
type Json = Aeson.Value

-- ** 'Json' pattern synonyms

pattern ObjectJson :: JsonObject -> Json
pattern ObjectJson object = Aeson.Object object

pattern ArrayJson :: JsonArray -> Json
pattern ArrayJson array = Aeson.Array array

pattern StringJson :: Text -> Json
pattern StringJson text = Aeson.String text

pattern NumberJson :: Scientific -> Json
pattern NumberJson scientific = Aeson.Number scientific

pattern BoolJson :: Bool -> Json
pattern BoolJson bool = Aeson.Bool bool

pattern NullJson :: Json
pattern NullJson = Aeson.Null

-- * Other aliases

type JsonObject = Aeson.KeyMap Json

type JsonObjectKey = Aeson.Key

type JsonArray = Vector Json
