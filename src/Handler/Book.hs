{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Book where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius

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
formBook :: Book -> [(Text, Key Category)] -> Form NewBook
formBook book categories = renderBootstrap3 BootstrapBasicForm $ NewBook
    <$> areq textField (FieldSettings "Nome do livro" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookName book)
    <*> areq textField (FieldSettings "ISBN" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookIsbn book)
    <*> areq intField (FieldSettings "Número de páginas" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookNumberPages book)
    <*> areq (selectFieldList categories) (FieldSettings "Categoria do Livro" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookCategoryId book)

mapCategories :: [Entity Category] -> [(Text, Key Category)]
mapCategories [] = []
mapCategories xs = map (\(Entity eid (Category name)) -> (name, eid)) xs

-- (map (\(Entity id (Category name)) -> (name, id))

getBookR :: Key Book -> Handler Html
getBookR bookid = do    
    book <- runDB $ get404 bookid
    categories <- runDB $ selectList [] [Desc CategoryId]
    (formWidget, _) <- generateFormPost $ formBook book $ mapCategories categories

    defaultLayout $ do
        msg <- getMessage
        session <- lookupSession "_ID"

        case session of
            Just _ -> do
                setTitle "BookRecorder - Cadastrar Livro"
                toWidgetBody $(juliusFile "templates/pages/book/show.julius")
                $(widgetFile "pages/book/show")
            Nothing -> redirect HomeR
    

postBookR :: Key Book -> Handler Html
postBookR bookid = do
    book <- runDB $ get404 bookid
    categories <- runDB $ selectList [] [Desc CategoryId]
    ((result, _), _) <- runFormPost $ formBook book $ mapCategories categories
    case result of
        FormSuccess book' -> do
            _ <- runDB $ update bookid [BookName =. (newBookName book'), BookIsbn =. (newBookISBN book'), BookNumberPages =. (newBookNumberPages book'), BookCategoryId =. (newBookCategory book')]
            redirect BookListR
            -- _ <- runDB $ insert user
        _ -> redirect $ BookR bookid

deleteBookR :: Key Book -> Handler Value
deleteBookR bookid = do
    book <- runDB $ get404 bookid
    _ <- runDB $ delete bookid
    returnJson book