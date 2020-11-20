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
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "BookRecorder - Home"
        $(widgetFile "pages/homepage")
