module Simple.Graphql.Query
  ( runQuery
  ) where

import Prelude

import Affjax (URL)
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow, try)
import Data.Array (catMaybes)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Effect.Aff (throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Simple.Graphql.Errors (GraphqlQueryError(..))
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
  => MonadThrow GraphqlQueryError m
  => URL
  -> Maybe String
  -> GraphQlQuery vs a 
  -> m (GraphQlQueryResponse a)
runQuery url mauth (GraphQlQuery gqlBody _) = post mauth url (Just gqlBody)
