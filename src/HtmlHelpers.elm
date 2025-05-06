module HtmlHelpers exposing (contentList, lazyContentList, maybeContentList, wrapToSingleNode, nothing)

{-| Helper functions for conditionally rendering HTML elements.

These utilities make it easier to work with [`Html`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#Html)
in a more declarative way.

@docs contentList, lazyContentList, maybeContentList, wrapToSingleNode, nothing

-}

import Html exposing (Html)
import Html.Attributes exposing (style)


{-| Conditionally render HTML nodes based on boolean flags.

This function is conceptually similar to `Attributes.classList`, but for
content nodes rather than CSS classes. Each entry in the list will be included
in the output only if its associated boolean value is `True`.

For example:

    viewAnimals : Animal -> Animal -> Animal -> Html msg
    viewAnimals dog cat mouse =
        Html.section [ Attributes.id "animals" ] <|
            contentList
                [ ( Html.text dog.name, dog.visible ) -- included only if dog.visible is True
                , ( Html.text cat.name, cat.visible ) -- included only if cat.visible is True
                , ( Html.text mouse.name, mouse.visible ) -- included only if mouse.visible is True
                ]

-}
contentList : List ( Html msg, Bool ) -> List (Html msg)
contentList contents =
    List.foldr
        (\( thunk, enabled ) acc ->
            if enabled then
                thunk :: acc

            else
                acc
        )
        []
        contents


{-| A lazy version of `contentList` for performance optimization.

This function takes a list of thunks (functions that take unit `()` and return HTML)
paired with boolean values. The thunks are evaluated only when their corresponding
boolean is `True`, making this function more efficient when the HTML generation is
expensive but condition evaluation is cheap.

Example:

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
    List.foldr
        (\( thunk, enabled ) acc ->
            if enabled then
                thunk () :: acc

            else
                acc
        )
        []
        contents


{-| Conditionally render HTML nodes based on `Maybe` values.

This function is similar to `lazyContentList` but works with `Maybe` values instead of
booleans. HTML nodes will only be rendered for items that have a `Just` value.
When the value is `Nothing`, the node is omitted from the output.

Example:

    view =
        maybeContentList
            [ ( \text -> Html.text text, Just "This will be rendered" )
            , ( \text -> Html.text text, Nothing ) -- This will be omitted
            , ( \user -> viewUser user, maybeUser ) -- Rendered only if maybeUser is Just
            ]

-}
maybeContentList : List ( a -> Html msg, Maybe a ) -> List (Html msg)
maybeContentList contents =
    contents
        |> List.foldr
            (\( thunk, maybeValue ) acc ->
                case maybeValue of
                    Just value ->
                        thunk value :: acc

                    Nothing ->
                        acc
            )
            []


{-| Convert a list of HTML nodes into a single node, following these rules:

1.  If the list is empty, return `nothing` (an empty text node)
2.  If the list contains exactly one element, return that element unchanged
3.  If the list contains multiple elements, wrap them with a `div` using `display: contents`

Using `display: contents` makes the wrapper transparent in the DOM layout,
allowing children to participate in the parent's layout as if they were direct children.

More about `display: contents`: <https://developer.mozilla.org/en-US/docs/Web/CSS/display#contents>

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


{-| A concise way to render nothing.

This is a convenience function that provides the idiomatic approach for rendering
nothing in Elm: an empty text node. Use this instead of `Html.text ""` for improved
readability and consistency.

-}
nothing : Html msg
nothing =
    Html.text ""


wrapperNode : List (Html msg) -> Html msg
wrapperNode =
    Html.div [ style "display" "contents" ]
