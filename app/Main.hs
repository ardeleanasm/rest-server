{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where




import Web.Spock
import Web.Spock.Config
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text,pack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (delete,get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (delete,get)
import           Database.Persist.TH
import Network.HTTP.Types.Status
import Lib

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json  
  name Text
  age Int
  deriving Show
 |]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a



main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
    runSpock 8080 (spock spockCfg app)

app :: Api
app = do
    get "people" $ do
        allPeople <- runSQL $ selectList [] [Asc PersonId]
        json allPeople
    get ("people" <//> var) $ \personId -> do
        maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> do 
                setStatus notFound404
                errorJson 2 "Could not find a person with matching id"
            Just thePerson -> json thePerson
    post "people" $ do
        maybePerson <- jsonBody :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> do 
                setStatus badRequest400
                errorJson 1 "Failed to parse request body as Person"
            Just thePerson -> do
                newId <- runSQL $ insert thePerson
                setStatus created201
                json $ object ["result" .= String "success", "id".=newId]
    put ("people" <//> var) $ \personId -> do
        maybePerson <- jsonBody
        case (maybePerson :: Maybe Person) of
            Nothing -> do
                setStatus badRequest400
                errorJson 1 "Failed to parse request body as Person"
            Just thePerson -> do
                runSQL $ replace personId thePerson
                setStatus created201
                json $ object ["result" .= String "success", "id".=personId]
    delete ("people" <//> var) $ \personId -> do
        runSQL $ P.delete (personId :: PersonId)
        setStatus noContent204
runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]
