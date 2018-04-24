module Main where

import Data.Array
import Data.Maybe                   (Maybe(..))
import Control.Monad.Eff            (Eff)
import Control.Monad.Eff.Console as C
import Prelude

import API                          (API(..), name, url)


main :: forall e. Eff ( console :: C.CONSOLE | e) Unit
main = do
    -- Create a new api
    let api = API { apiName: "test", headers: [], auth: Nothing, subResources: Just ["one","two"]}
    C.log $ name api 
    C.log $ url api Nothing

