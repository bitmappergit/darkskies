module Main where

import Affjax (get, printError)
import Affjax.ResponseFormat (json)
import Data.Argonaut (decodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Flame (Html, QuerySelector(..), Subscription)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as App
import Flame.Html.Attribute (class', id, onClick, onInput, placeholder)
import Flame.Html.Element (button, div, input, main, table_, tbody_, td_, text, tr_)
import Prelude (Unit, bind, show, (<>), ($), otherwise)

type Coord
  = { "lon" :: Number
    , "lat" :: Number
    }

type Weather
  = Array
      { "id" :: Number
      , "main" :: String
      , "description" :: String
      , "icon" :: String
      }

type Main
  = { "temp" :: Number
    , "feels_like" :: Number
    , "temp_min" :: Number
    , "temp_max" :: Number
    , "pressure" :: Int
    , "humidity" :: Int
    }

type Wind
  = { "speed" :: Number
    , "deg" :: Number
    }

type Clouds
  = { "all" :: Number
    }

type RainOrSnow
  = { "1h" :: Maybe Number
    , "3h" :: Maybe Number
    }

type Sys
  = { "country" :: Maybe String
    , "sunrise" :: Number
    , "sunset" :: Number
    }

type OpenWeatherMap
  = { "coord" :: Maybe Coord
    , "weather" :: Maybe Weather
    , "base" :: Maybe String
    , "main" :: Maybe Main
    , "visibility" :: Maybe Int
    , "wind" :: Maybe Wind
    , "clouds" :: Maybe Clouds
    , "rain" :: Maybe RainOrSnow
    , "snow" :: Maybe RainOrSnow
    , "dt" :: Maybe Number
    , "sys" :: Maybe Sys
    , "timezone" :: Maybe Number
    , "id" :: Maybe Number
    , "name" :: Maybe String
    }

type Model
  = { status :: Maybe String
    , currentWeather :: Maybe OpenWeatherMap
    , location :: String
    , apiKey :: String
    }

getWeather :: Model -> Aff (Model -> Model)
getWeather { location, apiKey } = do
  res <-
    get json $ "https://api.openweathermap.org/data/2.5/weather"
      <> "?q="
      <> location
      <> "&units=metric"
      <> "&appid="
      <> apiKey
  case res of
    Left err -> App.diff { status: Just ("GET /api response failed to decode: " <> printError err) }
    Right { body } -> do
      case decodeJson body of
        Left err -> App.diff { status: Just ("Can't parse JSON. " <> printJsonDecodeError err) }
        Right weather -> App.diff { status: Nothing, currentWeather: Just weather }

data Message
  = SetApiKey String
  | SetLocation String
  | GetWeather

init :: Tuple Model (Maybe Message)
init = Tuple model Nothing
  where
  model :: Model
  model =
    { status: Nothing
    , currentWeather: Nothing
    , location: ""
    , apiKey: ""
    }

update :: AffUpdate Model Message
update { model, message } = do
  case message of
    SetApiKey key -> App.diff { apiKey: key }
    SetLocation loc -> App.diff { location: loc }
    GetWeather -> getWeather model

formatWeather :: Model -> Html Message
formatWeather model
  | Just owm <- model.currentWeather
  , Just [ weather ] <- owm.weather
  , Just main <- owm.main
  , Just visibility <- owm.visibility =
    table_
      [ tbody_
          [ tr_ [ td_ "Forecast:", td_ (text weather.main) ]
          , tr_ [ td_ "Temperature:", td_ (show main.temp <> "°C") ]
          , tr_ [ td_ "Humidity:", td_ (show main.humidity <> "%") ]
          , tr_ [ td_ "Visibility:", td_ (show visibility <> "m") ]
          ]
      ]
  | otherwise =
    table_
      [ tbody_
          [ tr_ [ td_ "Error, could not get weather status." ]
          ]
      ]

view :: Model -> Html Message
view model =
  main "main"
    [ div [ class' "container", id "weather" ]
        [ div [ class' "row" ]
            [ div [ class' "column" ]
                [ formatWeather model
                ]
            ]
        , div [ class' "row" ]
            [ div [ class' "column" ]
                [ input [ placeholder "Location", onInput SetLocation ]
                , input [ placeholder "API Key", onInput SetApiKey ]
                , button [ onClick GetWeather ] "Get Weather"
                ]
            ]
        ]
    ]

subscribe :: Array (Subscription Message)
subscribe = []

application :: Effect Unit
application =
  App.mount_ (QuerySelector "body")
    { init: init
    , view: view
    , update: update
    , subscribe: subscribe
    }
