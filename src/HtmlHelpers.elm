module HtmlHelpers exposing (contentList, lazyContentList, maybeContentList, when, unless, lazyWhen, lazyUnless, maybeNode, resultNode, successNode, errNode, attributeIf, attributesIf, noAttribute, wrapToSingleNode, nothing, hideOnBreakpoint)

{-| Helper functions for conditionally rendering HTML elements.

These utilities make it easier to work with [`Html`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#Html)
in a more declarative way.

@docs contentList, lazyContentList, maybeContentList, when, unless, lazyWhen, lazyUnless, maybeNode, resultNode, successNode, errNode, attributeIf, attributesIf, noAttribute, wrapToSingleNode, nothing, hideOnBreakpoint

-}

import Html exposing (Html)
import Html.Attributes as Attributes


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


{-| Conditionally render a single HTML node.

If the condition is `True`, returns the provided node; otherwise returns `nothing`.

Example:

    view : Model -> Html msg
    view model =
        Html.section [ Attributes.id "notice" ]
            [ when model.showNotice (Html.text "Important notice") ]

-}
when : Bool -> Html msg -> Html msg
when cond view_ =
    if cond then
        view_

    else
        nothing


{-| Render a single HTML node when the condition is `False`.

This is the inverse of `when`.

Example:

    view : Model -> Html msg
    view model =
        Html.section [ Attributes.id "offline" ]
            [ unless model.isOnline (Html.text "You are offline") ]

-}
unless : Bool -> Html msg -> Html msg
unless cond view_ =
    when (not cond) view_


{-| Lazily render a single HTML node.

Takes a thunk `() -> Html msg` and evaluates it only when the condition is `True`.

Example:

    view : Model -> Html msg
    view model =
        Html.section [ Attributes.id "notice" ]
            [ lazyWhen model.showNotice (\_ -> expensiveNoticeView model) ]

-}
lazyWhen : Bool -> (() -> Html msg) -> Html msg
lazyWhen cond thunk =
    if cond then
        thunk ()

    else
        nothing


{-| Lazily render a single HTML node when the condition is `False`.

This is the inverse of `lazyWhen`.

Example:

    view : Model -> Html msg
    view model =
        Html.section [ Attributes.id "offline" ]
            [ lazyUnless model.isOnline (\_ -> expensiveOfflineBanner model) ]

-}
lazyUnless : Bool -> (() -> Html msg) -> Html msg
lazyUnless cond thunk =
    lazyWhen (not cond) thunk


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



-- (removed) maybe: prefer maybeNode for existing API
-- (removed) result/ifSuccess/ifError: prefer resultNode/successNode/errNode for existing API


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


{-| A no-op attribute.

This is effectively an empty `class` attribute. Useful as a placeholder when
you need to return a single attribute but want it to be a no-op.

Example:

    Html.div [ noAttribute ] []

-}
noAttribute : Html.Attribute msg
noAttribute =
    Attributes.class ""


{-| Conditionally include a single attribute.

Returns the given attribute when the condition is `True`, otherwise returns `noAttribute`.

Example:

    Html.button [ attributeIf model.disabled (Attributes.disabled True) ]
        [ Html.text "Submit" ]

-}
attributeIf : Bool -> Html.Attribute msg -> Html.Attribute msg
attributeIf cond attr =
    if cond then
        attr

    else
        noAttribute


{-| Conditionally include a list of attributes.

Returns the given attributes when the condition is `True`, otherwise returns an empty list.

Example:

    Html.div
        (attributesIf model.isPrimary
            [ Attributes.class "btn"
            , Attributes.class "btn-primary"
            ]
        )
        [ Html.text "Click" ]

-}
attributesIf : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
attributesIf cond attrs =
    if cond then
        attrs

    else
        []


wrapperNode : List (Html msg) -> Html msg
wrapperNode =
    Html.div [ Attributes.style "display" "contents" ]


{-| Render HTML based on a Maybe value.

This function takes a view function and a Maybe value. If the value is `Just`, it renders
the value using the provided view function. If the value is `Nothing`, it returns an empty
text node.

Example:

    viewUser : Maybe User -> Html msg
    viewUser maybeUser =
        maybeNode viewUserDetails maybeUser

-}
maybeNode : (a -> Html msg) -> Maybe a -> Html msg
maybeNode viewFn maybeValue =
    case maybeValue of
        Just value ->
            viewFn value

        Nothing ->
            nothing


{-| Render HTML based on a Result value.

This function takes two view functions - one for the error case and one for the success case -
and a Result value. It renders either the error or success view depending on the Result.

Example:

    viewUserData : Result String User -> Html msg
    viewUserData result =
        resultNode viewError viewUser result

-}
resultNode : (e -> Html msg) -> (a -> Html msg) -> Result e a -> Html msg
resultNode errorView successView res =
    case res of
        Ok value ->
            successView value

        Err error ->
            errorView error


{-| Render HTML for the success case of a Result.

This is a convenience function that only renders the success case of a Result,
returning an empty text node for the error case.

Example:

    viewUserData : Result String User -> Html msg
    viewUserData result =
        successNode viewUser result

-}
successNode : (a -> Html msg) -> Result e a -> Html msg
successNode viewFn res =
    case res of
        Ok value ->
            viewFn value

        Err _ ->
            nothing


{-| Render HTML for the error case of a Result.

This is a convenience function that only renders the error case of a Result,
returning an empty text node for the success case.

Example:

    viewError : Result String User -> Html msg
    viewError result =
        errNode viewErrorMessage result

-}
errNode : (e -> Html msg) -> Result e a -> Html msg
errNode viewFn res =
    case res of
        Ok _ ->
            nothing

        Err error ->
            viewFn error


{-| A clever (albeit hacky) helper to hide content (using max-width and max-height 0) below a given breakPoint.

Simply pipe your content like this: `|> hideOnBreakpoint "600px"` (or even `|> hideOnBreakpoint "100vh"`, to hide when width < height)

-}
hideOnBreakpoint : String -> Html msg -> Html msg
hideOnBreakpoint breakpoint content =
    let
        clampStyle : String
        clampStyle =
            "clamp(0px, calc((100vw - " ++ breakpoint ++ ") * 1000), 100vmax)"
    in
    Html.div
        [ Attributes.style "max-width" clampStyle
        , Attributes.style "max-height" clampStyle
        , Attributes.style "overflow" "hidden"
        ]
        [ content ]
