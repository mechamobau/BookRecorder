{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data UserLoginForm = UserLoginForm
    {
        email :: Text
    ,   password :: Text
    }

formUserLoginForm :: Form UserLoginForm
formUserLoginForm = renderBootstrap3 BootstrapBasicForm $ UserLoginForm
    <$> areq textField (FieldSettings "Email" Nothing Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq passwordField (FieldSettings "Senha" Nothing Nothing Nothing [("class", "form-control")]) Nothing

getLoginR :: Handler Html
getLoginR = do
    (formWidget, _) <- generateFormPost formUserLoginForm
    msg <- getMessage
    session <- lookupSession "_ID"
    defaultLayout $ do
        case session of
            Just _ -> redirect HomeR
            Nothing -> do
                setTitle "BookRecorder - Entrar"
                $(widgetFile "pages/login")

postLoginR :: Handler Html
postLoginR = do
    ((result, _), _) <- runFormPost formUserLoginForm
    case result of
        FormSuccess (UserLoginForm loginEmail loginPassword) -> do
            user' <- runDB $ getBy (UniqueEmail loginEmail)
            case user' of
                Just (Entity _ (User _ _ password' _)) -> do
                    if (loginPassword == password') then do
                        setSession "_ID" loginEmail
                        redirect DashboardR
                    else do
                        setMessage [shamlet|
                            <div .alert .alert-danger role=alert>
                                <p><strong>Oh não!</strong> Usuário não encontrado
                        |] 
                        redirect LoginR
                Nothing -> do
                    setMessage [shamlet|
                        <div .alert .alert-danger role=alert>
                            <p><strong>Oh não!</strong> Usuário não encontrado
                    |]
                    redirect LoginR
            -- _ <- runDB $ insert user
        _ -> redirect HomeR