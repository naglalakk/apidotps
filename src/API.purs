module API where

import Control.Monad.Eff            (Eff)
import Control.Promise as P
import Data.Array                   (cons, intercalate)
import Data.Either                  (Either(Left))
import Data.Maybe                   (Maybe(..), fromMaybe)
import Data.HTTP.Method 	    (Method(DELETE, PATCH, PUT, POST, GET))
import Data.String                  (toLower)
import Network.HTTP.Affjax as AJAX
import Network.HTTP.RequestHeader   (RequestHeader)
import Network.HTTP.Affjax.Request  (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Prelude
import Pluralize                    (pluralize)

data Authentication = Authentication {
    username :: String,
    password :: String
}

newtype API = API { 
    apiName :: String,
    headers :: Array RequestHeader,
    auth :: Maybe Authentication,
    subResources :: Maybe (Array String)
}

class RESTable a where 
    name :: a -> String
    url  :: a -> Maybe Int -> String
    apiRequest :: a -> AJAX.AffjaxRequest Unit
    get :: forall e b. Respondable b => 
                       a -> 
                       Maybe Int ->
                       Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse b))
    post :: forall e b c. Requestable b => 
                          Respondable c => 
                          a -> 
                          b -> 
                          Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse c))
    put :: forall e b c. Requestable b => 
                         Respondable c => 
                         a -> 
                         b -> 
                         Maybe Int ->
                         Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse c))
    patch :: forall e b c. Requestable b => 
                           Respondable c => 
                           a -> 
                           b -> 
                           Maybe Int ->
                          Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse c))
    delete :: forall e b. Respondable b => 
                          a -> 
                          Maybe Int ->
                          Eff ( ajax :: AJAX.AJAX | e) (P.Promise (AJAX.AffjaxResponse b))

instance defaultAPI :: RESTable API where
    name (API api) = pluralize (toLower api.apiName) 0 false
    apiRequest (API api) = do
        case (api.auth) of 
            Just (Authentication authorization) -> AJAX.defaultRequest 
                                  { headers  = api.headers, 
                                    username = Just authorization.username, 
                                    password = Just authorization.password, 
                                    withCredentials = true }
            Nothing -> AJAX.defaultRequest { headers = api.headers }
    url (API api) id = do
        let sID = fromMaybe 0 id
        let fID = if sID == 0 then "" else (show sID <> "/")
        let base = name (API api)
        case api.subResources of
            Just resources -> do
                "/" <> intercalate "/" (cons base resources) <> "/" <> fID
            Nothing ->
                "/" <> base <> "/" <> fID

    get api rID = P.fromAff $ AJAX.affjax $ (apiRequest api) { method = Left GET, url = url api rID}
    post api content = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                                 { method = Left POST, 
                                                   url = url api Nothing, 
                                                   content = Just content }
    put api content rID = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                                 { method = Left PUT, 
                                                   url = url api rID, 
                                                   content = Just content }

    patch api content rID = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                                 { method = Left PATCH, 
                                                   url = url api rID, 
                                                   content = Just content }

    delete api rID = P.fromAff $ AJAX.affjax $ (apiRequest api)
                                           { method = Left DELETE, 
                                             url = url api rID}

