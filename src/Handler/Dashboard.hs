{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Dashboard where

import Import

getDashboardR :: Handler Html
getDashboardR =
    defaultLayout $ do
    setTitle "BookRecorder - Dashboard"
    $(widgetFile "pages/dashboard")
