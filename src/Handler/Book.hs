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

import Handler.BookList (showBookCategory)
import Handler.BookNew  (FormBook (..), mapCategoryNames)

updateFormBook :: Book -> [(Text, Key Category)] -> Form FormBook
updateFormBook book categories = renderBootstrap3 BootstrapBasicForm $ FormBook
    <$> areq textField (FieldSettings "Nome do livro" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookName book)
    <*> areq textField (FieldSettings "ISBN" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookIsbn book)
    <*> areq intField (FieldSettings "Número de páginas" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookNumberPages book)
    <*> areq (selectFieldList categories) (FieldSettings "Categoria do Livro" Nothing Nothing Nothing [("class", "form-control")]) (Just $ bookCategory book)

getBookR :: Key Book -> Handler Html
getBookR bookid = do    
    book <- runDB $ get404 bookid
    categories <- runDB $ selectList [] [Desc CategoryId]
    (formWidget, _) <- generateFormPost $ updateFormBook book $ mapCategoryNames categories
    muser <- lookupSession "_ID"    
    case muser of
        Nothing -> redirect HomeR
        Just email -> do
            user   <- runDB $ selectFirst [UserEmail ==. email] []
            case user of
                Nothing -> redirect HomeR
                Just (Entity _ ( User _ _ _ isAdmin' )) ->                 
                    defaultLayout $ do
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
    ((result, _), _) <- runFormPost $ updateFormBook book $ mapCategoryNames categories

    case result of
        FormSuccess book' -> do
            _ <- runDB $ update bookid [BookName =. formBookName book', BookIsbn =. formBookISBN book', BookNumberPages =. formBookNumberPages book', BookCategory =. formBookCategory book']
            redirect BookListR
        _ -> redirect $ BookR bookid

deleteBookR :: Key Book -> Handler Value
deleteBookR bookid = do
    book <- runDB $ get404 bookid
    _ <- runDB $ delete bookid
    returnJson book