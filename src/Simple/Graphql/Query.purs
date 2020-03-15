module Simple.Graphql.Query
  ( runQuery
  ) where


import Affjax (URL)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Simple.Graphql.Errors (HttpRequestError)
import Simple.Graphql.Http (post)
import Simple.Graphql.Types (GraphQlQuery(..), GraphQlQueryResponse)
import Simple.JSON as JSON

-------------------------------------------------------------------------------
-- | runQuery
-------------------------------------------------------------------------------
-- | General function for querying a graphql api.
runQuery
  :: forall vs a m. 
     JSON.ReadForeign a
  => JSON.WriteForeign vs
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => URL
  -> Maybe String
  -> GraphQlQuery vs a 
  -> m (GraphQlQueryResponse a)
runQuery url mauth (GraphQlQuery gqlBody _) = post mauth url (Just gqlBody)
