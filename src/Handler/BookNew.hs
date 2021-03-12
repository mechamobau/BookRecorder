{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.BookNew where

import Import as I
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Lucius          (luciusFile)

data FormBook = FormBook {
        formBookName        :: Text
    ,   formBookISBN        :: Text
    ,   formBookNumberPages :: Int
    ,   formBookCategory    :: Key Category
}

formBook :: [(Text, Key Category)] -> Form FormBook
formBook categories = renderBootstrap3 BootstrapBasicForm $ FormBook
    <$> areq textField (FieldSettings "Nome do livro" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq textField (FieldSettings "ISBN" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq intField (FieldSettings "Número de páginas" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq (selectFieldList categories) (FieldSettings "Categoria do Livro" Nothing Nothing Nothing [("class", "form-control")]) Nothing

mapCategoryNames :: [Entity Category] -> [(Text, Key Category)]
mapCategoryNames [] = []
mapCategoryNames xs = map tupleNameWithId xs
    where tupleNameWithId (Entity eid (Category name)) = (name, eid)

getBookNewR :: Handler Html
getBookNewR = do    
    categories <- runDB $ selectList [] [Desc CategoryId]
    (formWidget, _) <- generateFormPost $ formBook $ mapCategoryNames categories

    defaultLayout $ do
        session <- lookupSession "_ID"

        case session of
            Just _ -> do
                setTitle "BookRecorder - Cadastrar Livro"
                toWidgetHead $(luciusFile "templates/default-layout-wrapper.lucius")
                $(widgetFile "pages/book/new")
            Nothing -> redirect HomeR
    

postBookNewR :: Handler Html
postBookNewR = do
    categories <- runDB $ selectList [] [Desc CategoryId]
    ((result, _), _) <- runFormPost $ formBook $ mapCategoryNames categories
    case result of
        FormSuccess book -> do
            book' <- runDB $ selectFirst [BookIsbn ==. (formBookISBN book)] []
            case book' of
                Just _ -> redirect BookNewR
                Nothing -> do
                    _ <- runDB $ insert400 (
                        Book 
                        (formBookName book) 
                        (formBookISBN book) 
                        (formBookNumberPages book) 
                        (Just $ pack "")
                        (formBookCategory book)
                        )
                    redirect BookListR
        _ -> redirect BookNewR
