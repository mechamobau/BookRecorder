{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)


formUserLoginForm :: Form (User, Text)
formUserLoginForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> (User 
        <$> areq textField (FieldSettings "Nome" Nothing Nothing Nothing [("class", "form-control")]) Nothing
        <*> areq textField (FieldSettings "Email" Nothing Nothing Nothing [("class", "form-control")]) Nothing
        <*> areq passwordField (FieldSettings "Senha" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    )
    <*> areq passwordField (FieldSettings "Confirmação de senha" Nothing Nothing Nothing [("class", "form-control")]) Nothing

getUserR :: Handler Html
getUserR = do
    (formWidget, _) <- generateFormPost formUserLoginForm
    msg <- getMessage
    session <- lookupSession "_ID"
    defaultLayout $ do
        case session of
            Just _ -> redirect HomeR
            Nothing -> do
                setTitle "BookRecorder - Entrar"
                addStylesheet $ StaticR css_bootstrap_css            
                $(widgetFile "pages/user")

postUserR :: Handler Html
postUserR = do
    ((result, _), _) <- runFormPost formUserLoginForm
    case result of
        FormSuccess (user, password_confirm) -> do
            user' <- runDB $ getBy (UniqueEmail $ userEmail user)
            case user' of
                Just _ -> do
                        setMessage [shamlet|
                            <div .alert .alert-warning role=alert>
                                <p><strong>Oops!</strong> E-mail fornecido já cadastrado na aplicação
                        |] 
                        redirect UserR
                Nothing -> do
                    if (password_confirm == userPassword user) then do
                        _ <- runDB $ insert400 user
                        setMessage [shamlet|
                            <div .alert .alert-success role=alert>
                                <p><strong>Sucesso!</strong> Você foi cadastrado com sucesso, para continuar insira seu e-mail e senha abaixo
                        |]
                        redirect LoginR
                    else do
                        setMessage [shamlet|
                            <div .alert .alert-warning role=alert>
                                <p><strong>Atenção!</strong> Senhas não coincidem
                        |]
                        redirect UserR
                    
            -- _ <- runDB $ insert user
        _ -> redirect HomeR