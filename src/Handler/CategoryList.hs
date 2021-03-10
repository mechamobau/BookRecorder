{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.CategoryList where

import Import

getCategoryListR :: Handler Html
getCategoryListR = do
    muser <- lookupSession "_ID"

    case muser of
        Nothing -> redirect HomeR
        Just _ -> do
            categories <- runDB $ selectList [] [Asc CategoryName]
            
            defaultLayout $ do

                setTitle "BookRecorder - Listar Categorias"

                $(widgetFile "pages/category/list")