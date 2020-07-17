module Pages.Designs.Uuid_String exposing (Model, Msg, Params, page)

import Dict
import Element exposing (..)
import Element.Font as Font
import Shared
import Shared.Design as Design
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- {{{ MODEL


type Model
    = AppNotRunning
    | LoadingNoStub String
    | LoadingWithStub String Design.DesignStub
    | UnknownDesignUuid String
    | Design String Design.Design



-- }}}
-- {{{ INIT


type alias Params =
    { uuid : String }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case Shared.getRunState shared of
        Just runState ->
            ( case
                Dict.get params.uuid runState.designs
                    |> Maybe.map Design.storedDesignToStub
              of
                Just stub ->
                    LoadingWithStub params.uuid stub

                Nothing ->
                    LoadingNoStub params.uuid
            , Cmd.none
            )

        Nothing ->
            ( AppNotRunning
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Design Details"
    , body = [ bodyView model ]
    }


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 12, width fill ]


bodyView : Model -> Element Msg
bodyView model =
    column [ width fill ]
        [ el [ centerX ] (Style.h1 <| text "Design Details")
        , case model of
            AppNotRunning ->
                sectionColumn
                    [ paragraph []
                        [ """An error has occurred while initialising the
                            application. In fact, you should never really be able to see
                            this message as the application should not get this far. Try
                            closing your browser and visiting the site again, or try a
                            different browser.
                            """
                            |> text
                        ]
                    ]

            LoadingNoStub uuidString ->
                sectionColumn [ text "Loading..." ]

            LoadingWithStub uuidString stub ->
                basicInformation stub

            UnknownDesignUuid uuidString ->
                sectionColumn
                    [ paragraph []
                        [ "Failed to load a design with UUID: "
                            |> text
                        , el [ Font.bold ] (text uuidString)
                        ]
                    , paragraph []
                        [ """This design no longer exists, you might have deleted
                            it or it may be stored on another computer. DESTRESS does
                            not store your designs on our server by default. If you'd
                            like this behaviour, you can opt in using the settings
                            panel.
                            """
                            |> text
                        ]
                    ]

            Design uuidString design ->
                sectionColumn
                    [ paragraph []
                        [ "Loaded design." |> text
                        ]
                    ]
        ]


basicInformation : { stubOrDesign | name : String, fileName : String } -> Element msg
basicInformation { name, fileName } =
    column
        [ width fill
        ]
        [ Style.h2 <| text ("Design Name: " ++ name)
        , Style.h3 <| text ("File: " ++ fileName)
        ]



-- }}}

