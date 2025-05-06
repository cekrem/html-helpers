# HTML Helpers for Elm

Utility functions for conditionally rendering HTML elements in Elm applications.

## Overview

This package provides helper functions that make it easier to:

- Conditionally render elements based on boolean flags with `contentList`
- Lazily render expensive HTML elements with `lazyContentList`
- Conditionally render based on `Maybe` values with `maybeContentList`
- Convert multiple HTML nodes into a single node with `wrapToSingleNode`
- Create empty nodes with the convenient `nothing` function

## Installation

```
elm install cekrem/html-helpers
```

## Usage

```elm
import HtmlHelpers exposing (contentList, lazyContentList, maybeContentList, wrapToSingleNode, nothing)

-- Conditionally render elements based on boolean flags
view =
    contentList
        [ ( Html.text "Always visible", True )
        , ( Html.text "Conditionally visible", model.isVisible )
        ]

-- See the module documentation for more examples
```

## Documentation

For complete documentation, see the [Elm package documentation](https://package.elm-lang.org/packages/cekrem/html-helpers/latest).

## Feedback and Contributions

Feedback, bug reports, and contributions are welcome! Please feel free to open an issue or submit a pull request.

## License

BSD-3-Clause
