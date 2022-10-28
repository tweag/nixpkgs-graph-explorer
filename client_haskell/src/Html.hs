{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Html where

import           Protolude
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as Hr
import qualified Data.Text.Lazy as Tl


newtype HtmlCmp = HtmlCmp
  { htmlBase :: Maybe Html -> Maybe Html -> Maybe Html -> Tl.Text
  }

newHtmlCmp :: HtmlCmp
newHtmlCmp = HtmlCmp
  { htmlBase = doc
  }


doc :: Maybe Html -> Maybe Html -> Maybe Html -> Tl.Text
doc contents results scripts =
  Hr.renderHtml $
    docTypeHtml $ do
      H.head $ do
          H.title "Nixpkgs Graph Explorer"
          meta
            ! charset "UTF-8"
          meta
            ! customAttribute "http-equiv" "X-UA-Compatible"
            ! content "IE=edge"
          meta
            ! name "viewport"
            ! content "width=device-width, initial-scale=1.0"
          H.link
            ! rel "stylesheet"
            ! href "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
            ! customAttribute "integrity" "sha512-EZLkOqwILORob+p0BXZc+Vm3RgJBOe1Iq/0fiI7r/wJgzOFZMlsqTa29UEl6v6U6gsV4uIpsNZoV32YZqrCRCQ=="
            ! customAttribute "crossorigin" "anonymous"
            ! customAttribute "referrerpolicy" "no-referrer"
          H.link
            ! rel "stylesheet"
            ! href "https://cdnjs.cloudflare.com/ajax/libs/tabulator/5.4.1/css/tabulator.min.css"
            ! customAttribute "integrity" "sha512-uXx6BpE3VCZ5j/eSrt2nUC4ZvIGiFx9JJD8gjiyi2wNBhczL31a8oCMPayARy/8pedUvjKkLw3VjmUyL9yRZkQ=="
            ! customAttribute "crossorigin" "anonymous"
            ! customAttribute "referrerpolicy" "no-referrer"
          H.link
            ! rel "stylesheet"
            ! href "css/src/cy.css"
          script
            ! src "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"
            $ mempty
          script
            ! src "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.23.0/cytoscape.min.js"
            ! customAttribute "integrity" "sha512-gEWKnYYa1/1c3jOuT9PR7NxiVI1bwn02DeJGsl+lMVQ1fWMNvtjkjxIApTdbJ/wcDjQmbf+McWahXwipdC9bGA=="
            ! customAttribute "crossorigin" "anonymous"
            ! customAttribute "referrerpolicy" "no-referrer"
            $ mempty
          script
            ! src "https://cdnjs.cloudflare.com/ajax/libs/tabulator/5.4.1/js/tabulator.min.js"
            ! customAttribute "integrity" "sha512-qgnv6utZN6s+TNg5iq42pPsSW/4qocwCM4d9CYmQArdJWtK7Qt1NmF+TNCsjCq136SEnhoMYjAQQF6KXtdlH4Q=="
            ! customAttribute "crossorigin" "anonymous"
            ! customAttribute "referrerpolicy" "no-referrer"
            $ mempty
          script
            ! src "https://cdnjs.cloudflare.com/ajax/libs/notify/0.4.2/notify.min.js"
            ! customAttribute "integrity" "sha512-efUTj3HdSPwWJ9gjfGR71X9cvsrthIA78/Fvd/IN+fttQVy7XWkOAXb295j8B3cmm/kFKVxjiNYzKw9IQJHIuQ=="
            ! customAttribute "crossorigin" "anonymous"
            ! customAttribute "referrerpolicy" "no-referrer"
            $ mempty
      body $ do
        H.div
          ! class_ "container" $ do
          H.div
            ! class_ "row" $ do
            H.div
              ! class_ "twelve columns" $ do
              H.div
                ! A.style "display: flex; flex-direction: row; align-items: center;" $ do
                img
                  ! src "img/line-chart.svg"
                  ! width "45"
                  ! height "45"
                h3
                  ! A.style "margin: 0;"
                  $ "Nixpkgs Graph Explorer"

          p mempty

          H.div
            ! class_ "row" $ do
            H.div
              ! class_ "twelve columns" $
                fromMaybe mempty contents
            H.div
              ! A.id "query-result"
              ! class_ "twelve columns no-result" $
                fromMaybe mempty results
        fromMaybe mempty scripts


queryForm :: Html
queryForm =
  H.div $
    H.form
      ! A.id "query-form" $ do
        H.div
          ! class_ "form-group" $ do
            H.label "Query"
            textarea
              ! A.id "input-query"
              ! class_ "form-control query-box"
              ! name "query"
              ! rows "8"
              ! cols "100"
              $ mempty
        button
          ! class_ "btn btn-default"
            $ "Submit"

queryRaw :: Html
queryRaw =
  H.div
    ! A.id "query-results-raw"
    ! class_ "row" $ do
      H.span "Raw Gremlin Python result:"
      H.div
        ! class_ "twelve columns" $
          H.textarea
            ! A.id "raw-data"
            ! rows "80"
            ! cols "100" $
              mempty


queryGraph :: Html
queryGraph =
  H.div
    ! A.id "query-results-graph"
    ! class_ "row" $
      H.div
        ! class_ "twelve columns" $
          H.div
            ! A.id "cy" $
              mempty

queryTable :: Html
queryTable =
  H.div
    ! A.id "query-results-table"
    ! class_ "row" $
      H.div
        ! class_ "twelve columns" $
          H.div
            ! A.id "table-graph" $
              mempty

queryJs :: Html
queryJs = H.script
  ! src "js/src/graph.js"
  $ mempty
