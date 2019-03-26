module Test.Main where

import Prelude

import Control.Monad.Error.Class (catchError, try)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (joinFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Simple.Graphql.Errors (handleError)
import Simple.Graphql.Query (runQuery)
import Simple.Graphql.Types (GraphQlQuery(..), runQueryT)
import Simple.Graphql.Utils (wrapDoubleQuotes)
import Type.Proxy (Proxy(..))



main :: Effect Unit
main = launchAff_ $ do
  let query = buildAllErc721Tokens
      url = "https://api.pixura.io/graphql"
  res <- runQueryT (runQuery url Nothing query)
  liftEffect $ log (show res)

-------------------------------------------------------------------------------
-- | AllErc721TokensResponse
-------------------------------------------------------------------------------
type AllErc721TokensResponse = {
  allErc721Tokens :: {
    nodes :: Array {
      tokenId :: Number,
      owner :: String,
      metadata :: {
        name :: String,
        description :: String,
        imageUri :: String
      }
    }
  }
}
-------------------------------------------------------------------------------
-- | buildAllErc721Tokens
-------------------------------------------------------------------------------
buildAllErc721Tokens :: GraphQlQuery AllErc721TokensResponse
buildAllErc721Tokens = GraphQlQuery { query: query } (Proxy :: Proxy AllErc721TokensResponse)
  where
    query = """
    {
    	allErc721Tokens(first:10) {
        nodes {
          tokenId
          owner
          metadata: erc721MetadatumByTokenId {
            name
            description
            imageUri
          }
        }
      }
    }
    """