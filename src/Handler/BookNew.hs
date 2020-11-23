{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.BookNew where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- Book
--     name                Text
--     isbn                Text
--     numberPages         Int
--     categoryId          CategoryId

data NewBook = NewBook {
        newBookName             :: Text
    ,   newBookISBN             :: Text
    ,   newBookNumberPages      :: Int
    ,   newBookCategory         :: Key Category
}

-- formBook :: Form NewBook
formBook :: [(Text, Key Category)] -> Form NewBook
formBook categories = renderBootstrap3 BootstrapBasicForm $ NewBook
    <$> areq textField (FieldSettings "Nome do livro" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq textField (FieldSettings "ISBN" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq intField (FieldSettings "Número de páginas" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq (selectFieldList categories) (FieldSettings "Categoria do Livro" Nothing Nothing Nothing [("class", "form-control")]) Nothing

mapCategories :: [Entity Category] -> [(Text, Key Category)]
mapCategories [] = []
mapCategories xs = map (\(Entity eid (Category name)) -> (name, eid)) xs

-- (map (\(Entity id (Category name)) -> (name, id))

getBookNewR :: Handler Html
getBookNewR = do    
    -- books <- runDB $ selectList ([] :: [Filter Book]) []
    categories <- runDB $ selectList [] [Desc CategoryId]
    -- muser <- lookupSession "_ID"
    (formWidget, _) <- generateFormPost $ formBook $ mapCategories categories

    defaultLayout $ do
        msg <- getMessage
        session <- lookupSession "_ID"

        case session of
            Just _ -> do
                setTitle "BookRecorder - Cadastrar Livro"
                $(widgetFile "pages/book/new")
            Nothing -> redirect HomeR
    

postBookNewR :: Handler Html
postBookNewR = do
    categories <- runDB $ selectList [] [Desc CategoryId]
    ((result, _), _) <- runFormPost $ formBook $ mapCategories categories
    case result of
        FormSuccess book -> do
            book' <- runDB $ selectFirst [BookIsbn ==. (newBookISBN book)] []
            case book' of
                Just _ -> redirect BookNewR
                Nothing -> do
                    _ <- runDB $ insert400 (Book (newBookName book) (newBookISBN book) (newBookNumberPages book) (newBookCategory book))
                    redirect BookListR
            -- _ <- runDB $ insert user
        _ -> redirect BookNewR
