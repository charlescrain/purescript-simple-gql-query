module Simple.Graphql.Utils 
  ( wrapDoubleQuotes
  ) where

import Prelude

-------------------------------------------------------------------------------
-- | wrapDoubleQuotes
-------------------------------------------------------------------------------
-- | wrap the string in escaped double quotes, useful for strings in GraphQl
--   queries.
wrapDoubleQuotes :: String -> String
wrapDoubleQuotes str = "\"" <> str <> "\""