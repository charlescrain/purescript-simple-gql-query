module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Simple.Graphql.Query (runQuery)
import Simple.Graphql.Types (GraphQlQuery(..), runQueryT)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main =
  launchAff_
    $ do
        let
          query = buildAllErc721Tokens
          expected = { continent: { code: "AF", name: "Africa" } }

          url = "https://countries.trevorblades.com"
        res <- runQueryT (runQuery url Nothing query)
        if res == { data: (Just expected), errors: Nothing }
          then pure unit
          else liftEffect $ throw $ show res

-------------------------------------------------------------------------------
-- | AllErc721TokensResponse
-------------------------------------------------------------------------------
type TestCountriesRes
  = { continent ::
        { code :: String
        , name :: String
        }
    }

-------------------------------------------------------------------------------
-- | buildAllErc721Tokens
-------------------------------------------------------------------------------
buildAllErc721Tokens :: GraphQlQuery { code :: String } TestCountriesRes
buildAllErc721Tokens = GraphQlQuery { query: query, variables: { code: "AF" } } (Proxy :: Proxy TestCountriesRes)
  where
  query =
    """
      query TestCountries($code: String) {
        continent(code: $code) {
            code
            name
        }
      }
    """
