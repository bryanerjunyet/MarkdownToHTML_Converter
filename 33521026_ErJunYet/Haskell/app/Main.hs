{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)
-- This file contains code that was generated with the assistance of ChatGPT and Copilot.
-- Some parts of the code were created or modified using these tools to improve efficiency and accuracy.

import           Assignment              (convertADTHTML, markdownParser, writeTextToFile)
import           Data.Aeson              (object, (.=))
import           Data.Aeson.Key          (fromString)
import           Data.Text.Lazy          (Text, pack, unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Instances               (ParseResult (Result), parse)
import           Web.Scotty              (ActionM, body, json, post, scotty)
import           Control.Monad.IO.Class  (liftIO)  -- Import liftIO


getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _            = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= ((pack value) :: Text) | (key, value) <- pairs]


main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse markdownParser str) (convertADTHTML)

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]

  post "/api/saveHTML" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
    -- Write the HTML to a file
    _ <- liftIO $ writeTextToFile str
    jsonResponse [("status", "HTML saved")]