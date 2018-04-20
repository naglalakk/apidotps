module API where

import Prelude
import Data.Either                  (Either(Left))
import Data.Maybe                   (Maybe(..))
import Data.HTTP.Method 	    (Method(DELETE, PATCH, PUT, POST, GET))
import Data.String                  (toLower)
import Control.Monad.Eff            (Eff)
import Control.Promise as P
import Network.HTTP.Affjax as AJAX
import Network.HTTP.RequestHeader   (RequestHeader)
import Network.HTTP.Affjax.Request  (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)

data Authentication = Authentication {
    username :: String,
    password :: String
}

newtype API = API { 
    apiName :: String,
    headers :: Array RequestHeader,
    auth :: Maybe Authentication
}

class RESTable a where 
    name :: a -> String
    url  :: a -> String
    apiRequest :: a -> AJAX.AffjaxRequest Unit
    get :: forall e b. Respondable b => 
                       a -> Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse b))
    post :: forall e b c. Requestable b => 
                          Respondable c => 
                          a -> 
                          b -> Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse c))
    put :: forall e b c. Requestable b => 
                         Respondable c => 
                         a -> 
                         b -> Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse c))
    patch :: forall e b c. Requestable b => 
                           Respondable c => 
                           a -> 
                           b -> 
                          Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse c))
    delete :: forall e b. Respondable b => 
                          a -> 
                          Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse b))

instance defaultAPI :: RESTable API where
    name (API api) = toLower api.apiName
    apiRequest (API api) = do
        case (api.auth) of 
            Just (Authentication authorization) -> AJAX.defaultRequest 
                                  { headers = (api.headers), 
                                    username = Just authorization.username, 
                                    password = Just authorization.password, 
                                    withCredentials = true }
            Nothing -> AJAX.defaultRequest { headers = api.headers }
    url api = "/" <> name api <> "/"
    get api = P.fromAff $ AJAX.affjax $ (apiRequest api) { method = Left GET, url = url api }
    post api content = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                                 { method = Left POST, 
                                                   url = url api, 
                                                   content = Just content }
    put api content = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                                 { method = Left PUT, 
                                                   url = url api, 
                                                   content = Just content }

    patch api content = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                                 { method = Left PATCH, 
                                                   url = url api, 
                                                   content = Just content }

    delete api = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                           { method = Left DELETE, 
                                             url = url api }
