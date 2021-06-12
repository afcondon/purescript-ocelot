module Ocelot.Block.Badge where

baseClasses :: Array HH.ClassName
baseClasses = HH.ClassName <$>
  [ "rounded-full"
  , "relative"
  , "before:no-content"
  , "before:w-full"
  , "before:h-full"
  , "before:absolute"
  , "before:pin-t"
  , "before:pin-l"
  , "flex"
  , "justify-center"
  , "items-center"
  , "bg-blue-88"
  , "text-white"
  ]

badgeClasses :: Array HH.ClassName
badgeClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "w-8"
    , "h-8"
    ]
  )

badgeSmallClasses :: Array HH.ClassName
badgeSmallClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "w-6"
    , "h-6"
    , "text-sm"
    ]
  )

badgeLargeClasses :: Array HH.ClassName
badgeLargeClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "w-12"
    , "h-12"
    ]
  )

module Ocelot.Block.Builder where


buttonSharedClasses :: Array HH.ClassName
buttonSharedClasses = HH.ClassName <$>
  [ "no-outline"
  , "px-4"
  , "py-2"
  , "!active:border-b"
  , "active:border-t"
  , "disabled:opacity-50"
  , "disabled:cursor-default"
  , "!disabled:cursor-pointer"
  ]

buttonMainClasses :: Array HH.ClassName
buttonMainClasses = buttonSharedClasses <>
  ( HH.ClassName <$>
    [ "rounded"
    ]
  )

buttonClasses :: Array HH.ClassName
buttonClasses = HH.ClassName <$>
  [ "bg-grey-50-a20"
  , "border-grey-50-a20"
  , "hover:!disabled:bg-grey-50-a30"
  , "focus:bg-grey-50-a30"
  , "text-black-20"
  ]

buttonPrimaryClasses :: Array HH.ClassName
buttonPrimaryClasses = HH.ClassName <$>
  [ "bg-blue-88"
  , "border-blue-88"
  , "hover:!disabled:bg-blue-82"
  , "focus:bg-blue-82"
  , "text-white"
  ]

buttonDarkClasses :: Array HH.ClassName
buttonDarkClasses = HH.ClassName <$>
  [ "bg-grey-70-a30"
  , "border-grey-70-a30"
  , "hover:!disabled:bg-grey-70-a40"
  , "focus:bg-grey-70-a40"
  , "text-white"
  ]

buttonClearClasses :: Array HH.ClassName
buttonClearClasses = HH.ClassName <$>
  [ "bg-transparent"
  , "border-transparent"
  , "text-grey-70"
  , "hover:text-grey-70-a30"
  , "focus:text-grey-70-a30"
  ]

buttonGroupClasses :: Array HH.ClassName
buttonGroupClasses = HH.ClassName <$>
  [ "flex"
  , "items-center"
  ]

centerClasses :: Array HH.ClassName
centerClasses = HH.ClassName <$>
  [ "mr-px"
  ]

leftClasses :: Array HH.ClassName
leftClasses = HH.ClassName <$>
  [ "mr-px"
  , "rounded-l"
  ]

rightClasses :: Array HH.ClassName
rightClasses = HH.ClassName <$>
  [ "rounded-r"
  ]


module Ocelot.Block.Card where

baseCardClasses :: Array HH.ClassName
baseCardClasses = HH.ClassName <$>
  [ "bg-white"
  , "mb-6"
  , "rounded"
  , "clearfix"
  ]

innerCardClasses :: Array HH.ClassName
innerCardClasses = HH.ClassName <$>
  [ "m-6"
  ]


module Ocelot.Block.Checkbox where

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "flex"
  , "flex-row"
  , "inline-block"
  , "py-2"
  , "cursor-pointer"
  , "text-black-20"
  , "items-center"
  , "text-left" -- styles get messed up otherwise
  ]

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  -- start shared custom classes defined for radios --
  [ "!disabled:sibling:bg-white"
  , "disabled:sibling:bg-grey-95"
  , "checked:sibling:before:opacity-100"
  , "checked:sibling:before:scale-1"
  , "checked:!disabled:sibling:border-blue-88"
  , "focus:sibling:border-blue-88"
  , "!checked:sibling:before:opacity-0"
  , "!checked:sibling:before:scale-0"
  , "!focus:hover:!checked:!disabled:sibling:border-grey-70"
  , "focus:sibling:shadow"
  , "checked:!disabled:sibling:before:bg-blue-88"
  , "checked:disabled:sibling:before:bg-grey-80"
  , "checked:disabled:sibling:border-grey-80"
  , "offscreen"
  -- end shared custom radio classes --
  , "checked:sibling:after:opacity-100"
  , "checked:sibling:after:scale-1"
  , "!checked:sibling:after:opacity-0"
  , "!checked:sibling:after:scale-0"
  ]

checkboxClasses :: Array HH.ClassName
checkboxClasses = HH.ClassName <$>
  [ "relative"
  , "content-box"
  , "border-2"
  , "border-solid"
  , "h-5"
  , "w-5"
  , "flex-none"
  , "no-content"
  , "mr-3"
  , "rounded"
  , "before:transition-1/4-bounce"
  , "before:absolute"
  , "before:h-full"
  , "before:w-full"
  , "before:no-content"
  , "after:transition-1/4-bounce"
  , "after:absolute"
  , "after:w-full"
  , "after:h-2"
  , "after:border-l-2"
  , "after:border-b-2"
  , "after:border-white"
  , "after:no-content"
  , "after:rotate-315"
  , "after:shadow"
  ]

choiceClasses :: Array HH.ClassName
choiceClasses = HH.ClassName <$>
  [ "absolute"
  , "bg-white"
  , "rounded-lg"
  , "border"
  , "border-grey-90"
  , "shadow"
  , "overflow-hidden"
  ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "flex"
  , "h-10"
  , "justify-center"
  , "items-center"
  , "border-b"
  , "border-grey-90"
  ]


optionClasses :: Array HH.ClassName
optionClasses = HH.ClassName <$>
  [ "bg-white"
  , "flex"
  , "flex-col"
  , "items-center"
  , "h-30"
  , "justify-center"
  , "w-40"
  , "cursor-pointer"
  ]

highlightedOptionClasses :: Array HH.ClassName
highlightedOptionClasses = HH.ClassName <$>
  [ "bg-grey-97" ]

module Ocelot.Block.Conditional where


percentBarContainerClasses :: Array Halogen.HTML.ClassName
percentBarContainerClasses =
  [ "inline-block"
  , "relative"
  ]
    <#> Halogen.HTML.ClassName

percentBarBackClasses :: Array Halogen.HTML.ClassName
percentBarBackClasses =
  [ "absolute"
  , "bg-grey-80"
  , "h-full"
  , "w-full"
  , "z-10"
  ]
    <#> Halogen.HTML.ClassName

percentBarFrontClasses :: Array Halogen.HTML.ClassName
percentBarFrontClasses =
  [ "absolute"
  , "h-full"
  , "z-20"
  ]
    <#> Halogen.HTML.ClassName

module Ocelot.Block.Expandable where

headingClasses :: Array HH.ClassName
headingClasses = HH.ClassName <$>
  [ "flex"
  , "justify-between"
  , "cursor-pointer"
  ]

headingInnerClasses :: Array HH.ClassName
headingInnerClasses = HH.ClassName <$>
  [ "flex-initial"
  ]

chevronClasses :: Array HH.ClassName
chevronClasses = HH.ClassName <$>
  [ "text-grey-70"
  , "text-lg"
  , "leading-loose"
  ]

contentSharedClasses :: Array HH.ClassName
contentSharedClasses = HH.ClassName <$>
  []

contentClasses :: Status -> Array HH.ClassName
contentClasses status_ = contentSharedClasses <>
  ( case status_ of
    Collapsed -> HH.ClassName <$>
      [ "max-h-0"
      , "opacity-0"
      , "overflow-hidden"
      , "transition-1/4-in"
      ]
    Expanded -> HH.ClassName <$>
      [ "max-h-full"
      , "opacity-1"
      , "transition-1/2-out"
      ]
  )

module Ocelot.Block.FormField where

fieldClasses :: Array HH.ClassName
fieldClasses = HH.ClassName <$>
  [ "w-full"
  , "mb-10"
  ]

helpTextClasses :: Array HH.ClassName
helpTextClasses = Format.mutedClasses <>
  ( HH.ClassName <$>
    [ "block"
    , "font-light"
    , "pt-3"
    ]
  )

errorTextClasses :: Array HH.ClassName
errorTextClasses = HH.ClassName <$>
  [ "block"
  , "text-red"
  , "font-medium"
  , "pt-3"
  ]

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "block"
  , "font-medium"
  , "leading-loose"
  , "text-black-20"
  ]


module Ocelot.Block.FormPanel where

buttonClasses :: Array HH.ClassName
buttonClasses = HH.ClassName <$>
  [ "font-medium"
  , "no-outline"
  , "text-blue-82"
  ]

module Ocelot.Block.Format where

headingClasses :: Array HH.ClassName
headingClasses = HH.ClassName <$>
  [ "mb-6"
  , "text-3xl"
  , "font-normal"
  , "leading-loose"
  , "flex"
  , "items-center"
  ]

headingDarkClasses :: Array HH.ClassName
headingDarkClasses = headingClasses <>
  ( HH.ClassName <$>
    [ "text-white"
    ]
  )

subHeadingClasses :: Array HH.ClassName
subHeadingClasses = HH.ClassName <$>
  [ "text-xl"
  , "font-medium"
  , "leading-loose"
  , "flex"
  , "items-center"
  , "mb-6"
  ]

subHeadingDarkClasses :: Array HH.ClassName
subHeadingDarkClasses = subHeadingClasses <>
  ( HH.ClassName <$>
    [ "text-white"
    ]
  )

contentHeadingClasses :: Array HH.ClassName
contentHeadingClasses = HH.ClassName <$>
  [ "mb-6"
  , "text-lg"
  , "font-normal"
  , "leading-loose"
  , "flex"
  , "items-center"
  ]

captionClasses :: Array HH.ClassName
captionClasses = HH.ClassName <$>
  [ "block"
  , "font-light"
  , "mb-6"
  , "text-grey-70"
  , "text-sm"
  , "tracking-wide"
  , "uppercase"
  ]

linkClasses :: Array HH.ClassName
linkClasses = HH.ClassName <$>
  [ "text-blue-75"
  , "hover:text-blue-65"
  , "no-underline"
  , "font-medium"
  , "cursor-pointer"
  ]

linkDarkClasses :: Array HH.ClassName
linkDarkClasses = HH.ClassName <$>
  [ "text-grey-light"
  , "hover:text-grey-lighter"
  , "no-underline"
  , "font-medium"
  , "cursor-pointer"
  ]

mutedClasses :: Array HH.ClassName
mutedClasses = HH.ClassName <$>
  [ "text-grey-50"
  ]

pClasses :: Array HH.ClassName
pClasses = HH.ClassName <$>
  [ "mb-6"
  ]


module Ocelot.Block.Header where

outerClasses :: Array HH.ClassName
outerClasses = HH.ClassName <$>
  [ "bg-black-10"
  , "w-full"
  , "px-6"
  ]

innerClasses :: Array HH.ClassName
innerClasses = HH.ClassName <$>
  [ "container"
  , "items-center"
  , "mx-auto"
  , "flex"
  , "h-24"
  ]


module Ocelot.Block.Hover where

hoverClasses :: Array HH.ClassName
hoverClasses = HH.ClassName <$>
  [ "absolute"
  , "invisible"
  , "group-hover:visible"
  , "z-60"
  ]

module Ocelot.Block.Icon where



module Ocelot.Block.Input where


inputSharedClasses :: Array HH.ClassName
inputSharedClasses = HH.ClassName <$>
  [ "bg-white"
  , "border-t-2"
  , "border-b-2"
  , "font-light"
  , "cc-blue-88"
  , "border-grey-80"
  , "disabled:bg-grey-95"
  , "disabled:text-grey-70"
  , "focus:no-outline"
  , "py-2"
  ]

inputClasses :: Array HH.ClassName
inputClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "border-l-2"
    , "border-r-2"
    , "w-full"
    , "px-3"
    , "focus:border-blue-88"
    , "!focus:!disabled:hover:border-grey-70"
    ]
  )

inputGroupClasses :: Array HH.ClassName
inputGroupClasses = HH.ClassName <$>
  [ "flex"
  , "group"
  , "w-full"
  ]

mainItemClasses :: Array HH.ClassName
mainItemClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "w-full"
    , "focus:border-blue-88"
    , "focus:sibling:border-blue-88"
    , "group-hover:!focus:!disabled:border-grey-70"
    , "group-hover:!focus:!disabled:sibling:border-grey-70"
    , "disabled:sibling:bg-grey-95"
    ]
  )

centerClasses :: Array HH.ClassName
centerClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "pl-1"
    , "pr-1"
    ]
  )

leftClasses :: Array HH.ClassName
leftClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "border-l-2"
    , "pl-3"
    , "pr-1"
    ]
  )

rightClasses :: Array HH.ClassName
rightClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "border-r-2"
    , "pr-3"
    , "pl-1"
    ]
  )

addonClasses :: Array HH.ClassName
addonClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "cursor-pointer"
    , "flex"
    , "items-center"
    , "text-grey-70"
    ]
  )

addonLeftClassess :: Array HH.ClassName
addonLeftClassess = addonClasses <> leftClasses <>
  ( HH.ClassName <$>
    [ "order-start"
    ]
  )

borderLeftClasses :: Array HH.ClassName
borderLeftClasses = borderClasses <>
  ( HH.ClassName <$>
    [ "border-r"
    , "pr-3"
    , "order-start"
    ]
  )

borderRightClasses :: Array HH.ClassName
borderRightClasses = borderClasses <>
  ( HH.ClassName <$>
    [ "border-l"
    , "pl-3"
    ]
  )

textareaClasses :: Array HH.ClassName
textareaClasses = inputClasses <>
  ( HH.ClassName <$>
    [ "min-h-40"
    ]
  )


module Ocelot.Block.ItemContainer where


menuClasses :: Array HH.ClassName
menuClasses = HH.ClassName <$>
  [ "bg-white"
  , "text-black-20"
  , "border"
  , "list-reset"
  , "rounded"
  , "shadow"
  , "absolute"
  , "z-60"
  , "min-w-50"
  ]

dropdownClasses :: Array HH.ClassName
dropdownClasses = menuClasses <>
  ( HH.ClassName <$>
    [ "absolute"
    , "pin-t-full"
    , "pin-l"
    , "max-h-160"
    , "overflow-y-auto"
    ]
  )

droprightClasses :: Array HH.ClassName
droprightClasses = menuClasses <>
  ( HH.ClassName <$>
    [ "absolute"
    , "pin-t"
    , "pin-l-full"
    ]
  )

baseClasses :: Array HH.ClassName
baseClasses = HH.ClassName <$>
  [ "bg-white"
  , "border-grey-80"
  , "border-l-2"
  , "border-r-2"
  , "w-full"
  ]

selectionContainerClasses :: Array HH.ClassName
selectionContainerClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "border-t-2"
    ]
  )

itemContainerClasses :: Array HH.ClassName
itemContainerClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "absolute"
    , "shadow"
    , "max-h-120"
    , "overflow-y-auto"
    , "z-50"
    , "border-b-2"
    , "pin-t-full"
    , "pin-l"
    ]
  )

ulClasses :: Array HH.ClassName
ulClasses = HH.ClassName <$> [ "list-reset" ]

liClasses :: Array HH.ClassName
liClasses = HH.ClassName <$>
  [ "px-4"
  , "py-2"
  , "rounded-sm"
  , "text-black-20"
  , "group"
  , "hover:bg-grey-97"
  , "cursor-pointer"
  ]

selectionGroupClasses :: Array HH.ClassName
selectionGroupClasses = HH.ClassName <$>
  [ "flex"
  , "items-start"
  , "justify-between"
  ]

buttonClasses :: Array HH.ClassName
buttonClasses = HH.ClassName <$>
  [ "invisible"
  , "text-grey-80"
  , "hover:text-grey-70"
  , "group-hover:visible"
  ]

module Ocelot.Block.Layout where

popoverClasses :: Array HH.ClassName
popoverClasses = HH.ClassName <$>
  [ "absolute"
  , "shadow"
  , "z-50"
  , "border"
  , "border-grey-90"
  , "rounded"
  ]

stickyClasses :: Array HH.ClassName
stickyClasses = HH.ClassName <$>
  [ "fixed"
  , "pin-t"
  , "pin-x"
  , "w-full"
  , "shadow-md"
  , "z-60"
  ]

containerClasses :: Array HH.ClassName
containerClasses = HH.ClassName <$>
  [ "container"
  , "m-auto"
  ]

gridClasses :: Array HH.ClassName
gridClasses = HH.ClassName <$>
  [ "container"
  , "m-auto"
  , "p-8"
  , "flex"
  ]

columnClasses :: Array HH.ClassName
columnClasses = HH.ClassName <$>
  [ "flex-1"
  , "p-8"
  ]

mainClasses :: Array HH.ClassName
mainClasses = HH.ClassName <$>
  [ "flex-3"
  , "p-8"
  ]

sideClasses :: Array HH.ClassName
sideClasses = HH.ClassName <$>
  [ "flex-2"
  , "p-8"
  ]


module Ocelot.Block.Loading where


module Ocelot.Block.Menu where


bodyClasses :: Array HH.ClassName
bodyClasses = HH.ClassName <$>
  [ "flex"
  , "flex-col"
  , "w-90"
  ]

optionClasses :: Array HH.ClassName
optionClasses = HH.ClassName <$>
  [ "bg-white"
  , "flex"
  , "items-center"
  , "justify-center"
  , "cursor-pointer"
  ]

highlightedOptionClasses :: Array HH.ClassName
highlightedOptionClasses = HH.ClassName <$>
  [ "bg-grey-97" ]

module Ocelot.Block.NavigationTab where


module Ocelot.Block.Pager where

pager :: ∀ p i. Int -> Int -> (Int -> MouseEvent -> i) -> HH.HTML p i
pager skip last query =
  HH.div
    [ HP.classes
      [ ClassName "cn-pagination"
      , ClassName "clearfix"
      ]
    ]
    [ HH.ul
      [ HP.class_ (ClassName "pagination")
      ]
      makePagingButtons
    ]
  where



module Ocelot.Block.Popover where

hoverClasses :: Array HH.ClassName
hoverClasses = HH.ClassName <$>
  [ "p-4"
  , "my-1"
  , "bg-white"
  , "border"
  , "border-grey-80"
  , "rounded"
  , "shadow-md"
  , "items-center"
  ]

module Ocelot.Block.Progress where

-- Passed props reach the <span>.

module Ocelot.Block.Radio where

import Prelude

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "flex"
  , "flex-row"
  , "inline-block"
  , "py-2"
  , "cursor-pointer"
  , "text-black-20"
  , "items-center"
  , "text-left" -- styles get messed up otherwise
  ]

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "!disabled:sibling:bg-white"
  , "disabled:sibling:bg-grey-95"
  , "checked:sibling:before:opacity-100"
  , "checked:sibling:before:scale-1"
  , "checked:!disabled:sibling:border-blue-88"
  , "focus:sibling:border-blue-88"
  , "!checked:sibling:before:opacity-0"
  , "!checked:sibling:before:scale-0"
  , "!focus:hover:!checked:!disabled:sibling:border-grey-70"
  , "focus:sibling:shadow"
  , "checked:!disabled:sibling:before:bg-blue-88"
  , "checked:disabled:sibling:before:bg-grey-80"
  , "checked:disabled:sibling:border-grey-80"
  , "offscreen"
  ]

radioClasses :: Array HH.ClassName
radioClasses = HH.ClassName <$>
  [ "inline-flex"
  , "justify-center"
  , "items-center"
  , "content-box"
  , "border-2"
  , "border-solid"
  , "h-4"
  , "w-4"
  , "p-1"
  , "flex-none"
  , "no-content"
  , "rounded-full"
  , "mr-3"
  , "before:transition-1/4-bounce"
  , "before:h-full"
  , "before:w-full"
  , "before:bg-blue-88"
  , "before:no-content"
  , "before:rounded-full"
  , "before:shadow"
  ]


module Ocelot.Block.Range (range) where

outerClasses :: Array HH.ClassName
outerClasses = HH.ClassName <$>
  [ "bg-black-10"
  , "w-full"
  ]

innerClasses :: Array HH.ClassName
innerClasses = HH.ClassName <$>
  [ "container"
  , "items-end"
  , "mx-auto"
  , "flex"
  , "h-16"
  , "list-reset"
  ]

tabClasses :: Array HH.ClassName
tabClasses = HH.ClassName <$>
  [ "pt-5"
  , "pb-6"
  , "inline-flex"
  , "no-underline"
  ]

activeTabClasses :: Array HH.ClassName
activeTabClasses = HH.ClassName <$>
  [ "border-b-2"
  , "border-blue-88"
  , "text-white"
  ]

inactiveTabClasses :: Array HH.ClassName
inactiveTabClasses = HH.ClassName <$>
  [ "border-b-2"
  , "border-black-10"
  , "hover:border-blue-88"
  , "hover:text-white"
  , "text-grey-70"
  ]

tabTextClasses :: Array HH.ClassName
tabTextClasses = HH.ClassName <$>
  [ "text-sm"
  , "tracking-wide"
  , "uppercase"
  , "bold"
  , "inline-flex"
  , "self-end"
  ]

errorIconClasses :: Array HH.ClassName
errorIconClasses = HH.ClassName <$>
  [ "text-2xl"
  , "text-red"
  , "mr-1"
  , "inline-flex"
  , "align-bottom"
  , "my-px"
  ]


module Ocelot.Block.Table where

tableClasses :: Array HH.ClassName
tableClasses = HH.ClassName <$>
  [ "w-full"
  , "text-left"
  , "border-collapse"
  ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "bg-grey-90"
  , "py-4"
  , "px-5"
  , "font-medium"
  , "text-black-20"
  ]

cellClasses :: Array HH.ClassName
cellClasses = HH.ClassName <$>
  [ "bg-white"
  , "p-5"
  , "min-h-20"
  , "border-b"
  , "border-grey-95"
  ]


module Ocelot.Block.Toast where

-- Necessary for centering the toast
toastContainerClasses :: Array HH.ClassName
toastContainerClasses = HH.ClassName <$>
  [ "flex"
  , "transition-1/4-in"
  , "transition-1/2-out"
  , "items-center"
  , "fixed"
  , "pin-l"
  , "pin-r"
  , "pin-b"
  , "z-10"
  ]

containerVisibleClasses :: Array HH.ClassName
containerVisibleClasses = HH.ClassName <$>
  [ "mb-8" ]

containerClosedClasses :: Array HH.ClassName
containerClosedClasses = HH.ClassName <$>
  [ "-mb-40" ]

toastClasses :: Array HH.ClassName
toastClasses = HH.ClassName <$>
  [ "shadow-md"
  , "p-4"
  , "ml-auto"
  , "mr-auto"
  , "items-center"
  , "border"
  , "border-grey-80"
  , "bg-white"
  , "rounded"
  , "flex"
  ]

module Ocelot.Block.Toggle (toggle) where

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "flex"
  , "flex-row"
  , "items-center"
  , "inline-block"
  , "py-1"
  , "cursor-pointer"
  , "leading-loose"
  , "text-black-20"
  ]

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "checked:sibling:bg-blue-88"
  , "checked:sibling:pl-5"
  , "!checked:sibling:bg-grey-80"
  , "!checked:sibling:pr-5"
  , "offscreen"
  ]

toggleClasses :: Array HH.ClassName
toggleClasses = HH.ClassName <$>
  [ "transition-1/8"
  , "inline-flex"
  , "justify-center"
  , "items-center"
  , "content-box"
  , "h-5"
  , "w-5"
  , "p-1"
  , "rounded-full"
  , "mr-3"
  , "before:bg-white"
  , "before:h-full"
  , "before:w-full"
  , "before:rounded-full"
  , "before:no-content"
  , "before:shadow"
  ]


module Ocelot.Block.ToggleButton where

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "checked:neighbor:bg-grey-50"
  , "checked:neighbor:text-white"
  , "checked:neighbor:border-grey-50"
  , "!checked:neighbor:hover:bg-grey-80"
  , "!checked:neighbor:hover:text-black-10!"
  , "offscreen"
  ]

toggleButtonClasses :: Array HH.ClassName
toggleButtonClasses = HH.ClassName <$>
  [ "no-outline"
  , "px-4"
  , "py-2"
  , "disabled:opacity-50"
  , "disabled:cursor-default"
  , "!disabled:cursor-pointer"
  , "bg-white"
  , "border-grey-80"
  , "border-2"
  , "focus:bg-grey-50-a30"
  , "text-black-20"
  , "inline-block"
  ]

module Ocelot.Block.Tooltip where

classesArr :: Array String
classesArr =
  [ "absolute"
  , "invisible"
  , "group-hover:visible"
  , "text-white"
  , "bg-grey-50"
  , "px-2"
  , "rounded"
  , "z-60"
  ]

module Ocelot.Block.Tray where

trayClasses :: Array HH.ClassName
trayClasses = HH.ClassName <$>
  [ "fixed"
  , "bg-white"
  , "p-6"
  , "pin-b"
  , "pin-l"
  , "shadow"
  , "border-t"
  , "border-grey-90"
  , "transition-1/4-out"
  , "w-full"
  , "flex"
  , "items-center"
  ]

module Ocelot.Components.MultiInput.Component where

closeButtonClasses :: Array Halogen.ClassName
closeButtonClasses =
  [ "!active:border-b"
  , "!disabled:cursor-pointer"
  , "active:border-t"
  , "align-middle"
  , "bg-transparent"
  , "border-transparent"
  , "disabled:cursor-default"
  , "disabled:opacity-50"
  , "focus:text-grey-70-a30"
  , "hover:text-grey-70-a30"
  , "no-outline"
  , "pl-1"
  , "text-grey-70"
  , "text-xs"
  ]
    <#> Halogen.ClassName

containerClasses :: Array Halogen.ClassName
containerClasses =
  [ "bg-white"
  , "border"
  , "px-2"
  , "rounded"
  , "w-full"
  ]
    <#> Halogen.ClassName

inputClasses :: Array Halogen.ClassName
inputClasses =
  [ "my-1"
  , "outline-none"
  , "px-1"
  ]
    <#> Halogen.ClassName

itemDisplayClasses :: Array Halogen.ClassName
itemDisplayClasses =
  [ "bg-grey-95"
  , "inline-block"
  , "m-1"
  , "px-2"
  , "rounded-lg"
  ]
    <#> Halogen.ClassName

module Ocelot.Components.MultiInput.TextWidth where

containerClasses :: Array Halogen.ClassName
containerClasses =
  [ "relative"
  ]
    <#> Halogen.ClassName

ghostClasses :: Array Halogen.ClassName
ghostClasses =
  [ "absolute"
  , "h-0"
  , "inline-block"
  , "invisible"
  , "overflow-hidden"
  , "pin-t"
  , "whitespace-no-wrap"
  ]
    <#> Halogen.ClassName

module Ocelot.Component.SearchBar where

   where
     containerClasses = HH.ClassName <$>
       [ "flex"
       , "no-outline"
       , "items-stretch"
       , "transition-1/4"
       , "border-b-2"
       , "group"
       ]

     containerCondClasses =
       ifOpen
         [ "max-w-160", "border-blue-88" ]
         [ "max-w-12", "border-transparent", "cursor-pointer" ]

     iconClasses = HH.ClassName <$>
       [ "pr-3"
       , "text-2xl"
       , "group-hover:text-grey-50"
       , "transition-1/4"
       ]

     iconCondClasses =
       ifOpen
         [ "text-grey-50", "mb-0", "mt-0" ]
         [ "text-grey-70", "-mb-1", "mt-1" ]

     inputClasses = HH.ClassName <$>
       [ "no-outline"
       , "flex-1"
       , "bg-transparent"
       , "h-full"
       , "transition-1/4"
       ]

     inputCondClasses =
       ifOpen
         [ "w-full" ]
         [ "w-0" ]

     buttonClasses = HH.ClassName <$>
       [ "no-outline"
       , "text-grey-70"
       , "hover:text-grey-50"
       , "text-xs"
       , "transition-1/4"
       , "flex-shrink"
       ]

     buttonCondClasses =
       ifOpen
         [ "opacity-100", "visible" ]
         [ "opacity-0", "invisible" ]

     hideClearClasses = HH.ClassName <$>
       if Data.String.null st.query then ["hidden"] else []

module Ocelot.Component.Tree where

where
    renderRow depth indexPath itemPath ix (Node { selected, expanded, children, value }) =
      [ HH.div
        [ css $ "flex border-b py-2 pr-2 " <> ("pl-" <> (show (depth * 10))) ]
        [ renderCarat children expanded path
        , HH.div
          [ css "inline-flex" ]
          [ Conditional.alt_ (checkable value)
            [ Checkbox.checkbox_
              [ HE.onChecked $ ToggleItem value itemPath (A.cons ix indexPath)
              , HP.checked selected
              ]
              [ HH.fromPlainHTML $ renderItem value ]
            ]
            [ HH.span
              [ css "cursor-pointer"
              , HE.onClick \_ -> ToggleChildren path
              ]
              [ HH.fromPlainHTML $ renderItem value ]
            ]
          ]
        ]
      ] <>
      ( if not expanded then [] else
        [ HH.div_
          ( A.concat
            $ A.mapWithIndex
              (renderRow (depth + 1) (A.cons ix indexPath) (A.snoc itemPath value))
              children
          )
        ]
      )
      where
        path = A.cons ix indexPath

    renderCarat children expanded path =
      carat
        [ HE.onClick \_ -> ToggleChildren path
        , css $ "mr-3 text-xl align-text-bottom cursor-pointer " <> visible
        ]
      where
        carat = if expanded then Icon.caratDown else Icon.caratRight
        visible = if A.length children > 0 then "visible" else "invisible"



module Ocelot.Data.Currency where


module Ocelot.Data.DateTime where



module Ocelot.Data.InputAcceptType where


module Ocelot.Data.IntervalTree where


module Ocelot.Data.Tree where



module Ocelot.DatePicker where

module Ocelot.DateTimePicker where

module Ocelot.Dropdown where


module Ocelot.FilePicker where


module Ocelot.HTML.Properties where

module Ocelot.Interface.Dropdown where

module Ocelot.Interface.Typeahead where



module Ocelot.Interface.Utilities where



module Ocelot.Part.Modal where

backgroundClasses :: Array HH.ClassName
backgroundClasses = HH.ClassName <$>
  [ "fixed"
  , "pin"
  , "bg-black-modal-a90"
  , "fade-in"
  , "z-10"
  , "overflow-y-auto"
  ]

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "absolute"
  , "pin-x"
  , "pin-t"
  , "my-20"
  , "m-auto"
  , "max-w-lg"
  , "slide-down"
  , "z-10"
  ]

bodyClasses :: Array HH.ClassName
bodyClasses = HH.ClassName <$>
  [ "relative"
  , "bg-grey-95"
  , "overflow-auto"
  , "max-h-full"
  , "w-full"
  , "flex-col"
  , "flex"
  , "rounded-b"
  ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "h-24"
  , "flex"
  ]

outerHeaderClasses :: Array HH.ClassName
outerHeaderClasses = HH.ClassName <$>
  [ "bg-white"
  , "w-full"
  , "px-6"
  , "items-center"
  , "flex"
  , "rounded-t"
  ]

innerHeaderClasses :: Array HH.ClassName
innerHeaderClasses = HH.ClassName <$>
  [ "w-full"
  , "items-center"
  , "mx-auto"
  , "flex"
  ]

module Ocelot.Part.Panel where

backgroundClasses :: Array Halogen.HTML.ClassName
backgroundClasses =
  [ "bg-black-modal-a90"
  , "fade-in"
  , "fixed"
  , "pin"
  , "z-10"
  ]
    <#> Halogen.HTML.ClassName

backgroundOpenClasses :: Array Halogen.HTML.ClassName
backgroundOpenClasses =
  [ "opacity-1"
  , "visible"
  ]
    <#> Halogen.HTML.ClassName

backgroundClosedClasses :: Array Halogen.HTML.ClassName
backgroundClosedClasses =
  [ "invisible"
  , "opacity-0"
  ]
    <#> Halogen.HTML.ClassName

panelClasses :: Array Halogen.HTML.ClassName
panelClasses =
  [ "absolute"
  , "h-full"
  , "overflow-y-auto"
  , "transition-1/2-out"
  , "z-10"
  ]
    <#> Halogen.HTML.ClassName

panelLeftClasses :: Array Halogen.HTML.ClassName
panelLeftClasses =
  [ "pin-l"
  ]
    <#> Halogen.HTML.ClassName

panelLeftCss :: Foreign.Object.Object String
panelLeftCss =
  Foreign.Object.fromHomogeneous
    { transform: "translate(-300px)"
    }

panelRightClasses :: Array Halogen.HTML.ClassName
panelRightClasses =
  [ "pin-r"
  ]
    <#> Halogen.HTML.ClassName

panelRightCss :: Foreign.Object.Object String
panelRightCss =
  Foreign.Object.fromHomogeneous
    { transform: "translate(300px)"
    }

bodyClasses :: Array Halogen.HTML.ClassName
bodyClasses =
  [ "bg-grey-95"
  , "flex"
  , "flex-col"
  , "rounded-b"
  , "w-full"
  ]
    <#> Halogen.HTML.ClassName

headerClasses :: Array Halogen.HTML.ClassName
headerClasses =
  [ "h-24"
  , "flex"
  ]
    <#> Halogen.HTML.ClassName

outerHeaderClasses :: Array Halogen.HTML.ClassName
outerHeaderClasses =
  [ "bg-white"
  , "w-full"
  , "px-6"
  , "items-center"
  , "flex"
  , "rounded-t"
  ]
    <#> Halogen.HTML.ClassName

innerHeaderClasses :: Array Halogen.HTML.ClassName
innerHeaderClasses =
  [ "w-full"
  , "items-center"
  , "mx-auto"
  , "flex"
  ]
    <#> Halogen.HTML.ClassName


module Ocelot.Slider where


module Ocelot.Slider.Render where

module Ocelot.TimePicker where

dropdownClasses :: Array HH.ClassName
dropdownClasses = HH.ClassName <$>
  [ "max-h-80"
  , "w-full"
  , "overflow-y-scroll"
  , "pin-t"
  , "pin-l"
  , "bg-white"
  , "text-center"
  ]



module Ocelot.Typeahead where

disabledClasses :: Array Halogen.HTML.ClassName
disabledClasses = Halogen.HTML.ClassName <$>
  [ "bg-grey-95"
  , "text-grey-70"
  , "sibling:bg-grey-95"
  , "sibling:text-grey-50"
  , "border-t-2"
  , "border-b-2"
  , "font-light"
  , "focus:no-outline"
  , "py-2"
  , "border-l-2"
  , "w-full"
  , "px-3"
  ]

