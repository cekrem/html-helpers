module HtmlHelpers exposing (contentList, wrapToSingleNode, nothing)

{-| Convenient helper functions for working with
[`Html`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#Html)

@docs contentList, wrapToSingleNode, nothing

-}

import Html exposing (Html)
import Html.Attributes exposing (style)


{-| This function makes it easier to conditionally render Html.nodes.
The most obvious usecase is similar to that of Attributes.classList,
but for content rather than classes. Each entry in the list will be added
and removed depending on the boolean value it is paired with.

For example, maybe we want to render a few animals:

    viewAnimals : Animal -> Animal -> Animal -> Html msg
    viewAnimals dog cat mouse =
        Html.section [ Attributes.id "animals" ] <|
            contentList
                [ ( Html.text dog.name, dog.visible ) -- dog will be rendered if dog.visible is True
                , ( Html.text cat.name, cat.visible ) -- etc
                , ( Html.text mouse.name, mouse.visible ) -- etc
                ]

-}
contentList : List ( Html msg, Bool ) -> List (Html msg)
contentList contents =
    contents |> List.filter Tuple.second |> List.map Tuple.first


{-| wrapToSingleNode turns a list of Html nodes into a single node, using the following logic:

  - if the list is empty, return `nothing`
  - if the list contains only one element, simply return that element
  - if the list contains multiple elements, wrap them with a plain div with `display: contents`

More on `display: contents` here: <https://developer.mozilla.org/en-US/docs/Web/CSS/display#contents>

-}
wrapToSingleNode : List (Html msg) -> Html msg
wrapToSingleNode contents =
    case contents of
        [] ->
            nothing

        [ singleNode ] ->
            singleNode

        nodes ->
            wrapperNode nodes


{-| nothing is a convenience function for the most idiomatic and common way to render nothing,
namely an empty Html.text.
-}
nothing : Html msg
nothing =
    Html.text ""


wrapperNode : List (Html msg) -> Html msg
wrapperNode =
    Html.div [ style "display" "contents" ]
