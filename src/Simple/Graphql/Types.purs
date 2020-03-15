module Simple.Graphql.Types 
  ( GraphqlBody(..)
  , GraphQlQuery(..)
  , GraphQlQueryResponse(..)
  , GraphQlQueryResponseError(..)
  , EmptyResponse(..)
  , QueryT(..)
  , runQueryT
  ) where


import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Simple.Graphql.Errors (HttpRequestError, handleError)
import Simple.JSON (class ReadForeign)
import Type.Proxy (Proxy)

-------------------------------------------------------------------------------
-- | QueryT
-------------------------------------------------------------------------------
newtype QueryT a = QueryT (ExceptT HttpRequestError Aff a)

derive newtype instance functorQueryT :: Functor QueryT
derive newtype instance applyQueryT :: Apply QueryT
derive newtype instance applicativeQueryT :: Applicative QueryT
derive newtype instance bindQueryT :: Bind QueryT 
derive newtype instance monadQueryT :: Monad QueryT 
derive newtype instance monadThrowQueryT :: MonadThrow HttpRequestError QueryT
derive newtype instance monadErrorQueryT :: MonadError HttpRequestError QueryT
derive newtype instance monadEffectQueryT :: MonadEffect QueryT
derive newtype instance monadAffQueryT :: MonadAff QueryT

runQueryT :: forall a. QueryT a -> Aff a
runQueryT (QueryT f) = (errH =<< (runExceptT f))
  where 
    errH = either handleError pure

-------------------------------------------------------------------------------
-- | GraphqlBody
-------------------------------------------------------------------------------
type GraphqlBody vs = {
  query :: String,
  variables :: vs
}

-------------------------------------------------------------------------------
-- | GraphQlQuery
-------------------------------------------------------------------------------
data GraphQlQuery vs a  = GraphQlQuery (GraphqlBody vs) (Proxy a)

derive instance genericGraphQlQuery :: Generic (GraphQlQuery vs a) _

instance showGraphQlQuery :: (Show a, Show vs) => Show (GraphQlQuery vs a) where
  show = genericShow

instance eqGraphQlQuery :: (Eq a, Eq vs) => Eq (GraphQlQuery vs a) where
  eq = genericEq

-------------------------------------------------------------------------------
-- | GraphQlQueryResponseError
-------------------------------------------------------------------------------
type GraphQlQueryResponseError =  {
  message :: String,
  locations :: Array {
    line :: Int,
    column :: Int
  },
  path :: Array String
}

-------------------------------------------------------------------------------
-- | GraphQlQueryResponse
-------------------------------------------------------------------------------
type GraphQlQueryResponse a =  {
  data :: Maybe a,
  errors :: Maybe (Array GraphQlQueryResponseError)
}

-------------------------------------------------------------------------------
-- | EmptyResponse
-------------------------------------------------------------------------------
newtype EmptyResponse = EmptyResponse Unit
instance readEmptyResponse :: ReadForeign EmptyResponse where
  readImpl = const (pure $ EmptyResponse unit)

