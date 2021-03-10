{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.CategoryNew where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

formCategory :: Form Category
formCategory = renderBootstrap3 BootstrapBasicForm $ Category
    <$> areq textField (FieldSettings "Nome da categoria" Nothing Nothing Nothing [("class", "form-control")]) Nothing

getCategoryNewR :: Handler Html
getCategoryNewR = do
    (formWidget, _) <- generateFormPost formCategory

    session <- lookupSession "_ID"

    case session of
        Nothing -> redirect HomeR
        Just _ -> 
            defaultLayout $ do
                setTitle "BookRecorder - Cadastrar Categoria"
                $(widgetFile "pages/category/new")

postCategoryNewR :: Handler Html
postCategoryNewR = do
    ((result, _), _) <- runFormPost formCategory

    case result of
        FormSuccess (Category name) -> do
            _ <- runDB $ insert400 $ Category name
            redirect CategoryListR
        
        _ -> redirect HomeR
