{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = 
    defaultLayout $ do
        session <- lookupSession "_ID"
        case session of
            Just _ -> redirect DashboardR
            Nothing -> do
                setTitle "BookRecorder - Home"
                $(widgetFile "pages/homepage")

getRedirectAppR :: Handler Html
getRedirectAppR = redirect HomeR
