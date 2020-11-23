{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.BookList where

import Import

getBookListR :: Handler Html
getBookListR = do    
    books <- runDB $ selectList [] [Asc BookName]
    muser <- lookupSession "_ID"    
    case muser of
        Nothing -> redirect HomeR
        Just email -> do
            user   <- runDB $ selectFirst [UserEmail ==. email] []
            case user of
                Nothing -> redirect HomeR
                Just (Entity _ ( User _ _ _ isAdmin' )) -> 
                    defaultLayout $ do
                        setTitle "BookRecorder - Livros"
                        $(widgetFile "pages/book/list")


showBookCategory :: Book -> Widget
showBookCategory book = do
    (Category name) <- handlerToWidget $ runDB $ get404 $ bookCategoryId book
    [whamlet|
        #{name}
    |]