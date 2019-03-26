module Simple.Graphql.Errors 
  ( GraphqlQueryError(..)
  , handleError
  ) where


import Prelude

import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Error, error)


-------------------------------------------------------------------------------
-- | Errors
-------------------------------------------------------------------------------
-- | All possible errors within the relayer
data GraphqlQueryError = InvalidJsonBody String
                  | HttpError StatusCode String
                  | HttpResponseFormatError String
                  | HttpConnectionError String
                  | GraphQlNoDataError String
                  | GraphQlShouldNotError String
                  | NotValidEthereumAddress String
                  | InvalidSignedOrder String

derive instance eqGraphqlQueryError :: Eq GraphqlQueryError
derive instance genericGraphqlQueryError :: Generic GraphqlQueryError _
instance showInsertGraphqlQueryError :: Show GraphqlQueryError where
  show = genericShow

-------------------------------------------------------------------------------
-- | handleError
-------------------------------------------------------------------------------
-- | General error handler for the GraphqlQueryError. Maps from GraphqlQueryError to 
--   Error.
handleError :: forall a m. (MonadError Error m) => GraphqlQueryError -> m a
handleError (HttpError (StatusCode sc) err) = throwError <<< error $ "Http Error, status code: " <> show sc <> ", Error: " <> err
handleError (HttpResponseFormatError err) = throwError <<< error $ "Failed to format http repsonse: " <> err
handleError (HttpConnectionError err) = throwError <<< error $ "Failed to connect to remote host: " <> err
handleError (InvalidJsonBody err) = throwError <<< error $ "Failed to parse Json body: " <> err
handleError (GraphQlNoDataError err) = throwError <<< error $ "No data field returned from GraphQlQuery, errors found: " <> err
handleError (GraphQlShouldNotError err) = throwError <<< error $ "Found errors in a response that does not permit errors, errors found: " <> err
handleError (NotValidEthereumAddress err) = throwError <<< error $ "Invalid ethereum address. Must be a 40 character hex string: " <> err
handleError (InvalidSignedOrder err) = throwError <<< error $ "Invalid signed order: " <> err

