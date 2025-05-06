module HtmlHelpers exposing (contentList, lazyContentList, wrapToSingleNode, nothing, maybeContentList)

{-| Convenient helper functions for working with
[`Html`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#Html)

@docs contentList, lazyContentList, wrapToSingleNode, nothing, maybeContentList

-}

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy, lazy2)


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


{-| A lazy version of contentList. This version takes a list of thunks paired with
boolean values. The thunks are only evaluated for items that will be rendered
(when their bool is True).

This is useful when the Html nodes are expensive to compute but the conditions are cheap.

For example:

    viewAnimals : List Animal -> Html msg
    viewAnimals animals =
        Html.section [ Attributes.id "animals" ] <|
            lazyContentList
                [ ( \_ -> expensiveRenderFunction animal1, animal1.visible )
                , ( \_ -> expensiveRenderFunction animal2, animal2.visible )
                , ( \_ -> expensiveRenderFunction animal3, animal3.visible )
                ]

-}
lazyContentList : List ( () -> Html msg, Bool ) -> List (Html msg)
lazyContentList contents =
    contents
        |> List.filter Tuple.second
        |> List.map (\( thunk, _ ) -> lazy thunk ())


{-| This function works similar to lazyContentList but for Maybe values.
It will only render items that have a Just value (of any type) associated with them.
It uses lazy2 for memoization of both the function and its parameter.

Example usage:

    myView =
        maybeContentList
            [ ( \text -> Html.text text, Just "this will render" )
            , ( \text -> Html.text text, Nothing ) -- this will not render
            ]

-}
maybeContentList : List ( a -> Html msg, Maybe a ) -> List (Html msg)
maybeContentList contents =
    contents
        |> List.foldr
            (\( thunk, maybeValue ) acc ->
                case maybeValue of
                    Nothing ->
                        acc

                    Just value ->
                        lazy2 (\fn val -> fn val) thunk value :: acc
            )
            []


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
