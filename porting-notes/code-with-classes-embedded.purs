field
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
field config iprops html =
  field'
    config
    iprops
    ( HH.div [ css "my-1" ] html )


fieldSmall
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldSmall config iprops html =
  field'
    config
    iprops
    ( HH.div [ css "my-1 md:w-1/4" ] html )

fieldMid
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldMid config iprops html =
  field'
    config
    iprops
    ( HH.div [ css "my-1 md:w-1/2" ] html )

fieldset
  :: ∀ p i
   . FieldConfig p i
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
fieldset config iprops html =
  HH.div
    ( [ HP.classes fieldClasses ] <&> iprops )
    [ HH.fieldset
      []
      [ HH.legend
        [ HP.classes labelClasses ]
        [ HH.fromPlainHTML config.label ]
      , HH.div
        [ css "my-1" ]
        html
      , error_ config.error
      , helpText_ config.helpText
      ]
    ]


formPanel
  :: ∀ p i
   . FormPanelProps
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
formPanel props iprops html =
  HH.div
    [ HP.class_ (HH.ClassName "w-full") ]
    [ HH.div
        [ HP.classes contentClasses ]
        html
    , HH.button
        (iprops <> [ HP.classes buttonClasses ])
        [ (HH.fromPlainHTML (props.renderToggle props.isOpen)) ]
    ]
  where
    contentClasses =
      if props.isOpen
        then [ HH.ClassName "mb-6" ]
        else [ HH.ClassName "hidden" ]

stickyFormHeader
  :: ∀ p i page
   . Eq page
  => FormHeaderProps p i
  -> NavigationTab.TabConfig page
  -> HH.HTML p i
stickyFormHeader hConfig tConfig =
  HH.div
    [ HP.class_ $ HH.ClassName "h-40" ]
    [ HH.div
      [ HP.classes Layout.stickyClasses ]
      [ formHeader hConfig
      , NavigationTab.navigationTabs tConfig [ HP.class_ $ HH.ClassName "px-16" ]
      ]
    ]

stickyHeader_ :: ∀ p i. FormHeaderProps p i -> HH.HTML p i
stickyHeader_ config =
  HH.div
    [ HP.class_ $ HH.ClassName "h-24" ]
    [ HH.div
      [ HP.classes Layout.stickyClasses ]
      [ formHeader config ]
    ]

formHeader :: ∀ p i. FormHeaderProps p i -> HH.HTML p i
formHeader props =
  header_ $
    case props.brand of
      Just src  ->
        [ HH.div
          [ css "w-16" ]
          [ HH.img [ HP.src src ] ]
        ]
      Nothing -> []
    <>
    [ HH.h2
      [ css "flex-1 font-medium" ]
        [ HH.span
            [ css "text-lg text-grey-70 mr-4" ]
            props.name
        , HH.span
            [ css "text-lg text-white" ]
            props.title
        ]
    ]
    <>
    props.buttons

hover
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array HoverAnchor
  -> HH.HTML p i
  -> HH.HTML p i
  -> HH.HTML p i
hover props anchors html hoverHtml =
  HH.div
    ([ css $ "inline-block group cursor-pointer relative " ] <&> props)
    [ HH.div
      [ HP.classes $ hoverClasses <> anchorClasses anchors ]
      [ hoverHtml ]
    , html
    ]
  where
    anchorClasses :: Array HoverAnchor -> Array HH.ClassName
    anchorClasses = map $ HH.ClassName <<< anchorClass
    anchorClass Left = "pin-r-full"
    anchorClass Right = "pin-l-full"
    anchorClass Top = "pin-b-full"
    anchorClass Bottom = "pin-t-full"


dropdownButton
  :: ∀ p r i
   . (Array (IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i)
  -> Array (IProp r i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
dropdownButton button iprops html =
  button
    ( [ css "font-medium flex items-center" ] <&> iprops )
    $ html <> [ Icon.caratDown [ css "ml-3 text-xs" ] ]

dropdownItem
  :: ∀ p r i
   . (Array (IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i)
  -> Array (IProp r i)
  -> Array (HH.HTML p i)
  -> Boolean
  -> Boolean
  -> HH.HTML p i
dropdownItem elem props html selected highlighted =
  elem
    ( props <&> [ HP.classes itemClasses ] )
    $ [ Icon.selected [ HP.classes checkmarkClass ] ] <> html
  where
    itemClasses :: Array HH.ClassName
    itemClasses =
      liClasses
      <> [ HH.ClassName "flex" ]
      <> ( if highlighted then [ HH.ClassName "bg-grey-lighter" ] else [] )
      <> if selected then [ HH.ClassName "font-medium" ] else []

    checkmarkClass :: Array HH.ClassName
    checkmarkClass =
      (HH.ClassName <$> [ "mr-2", "text-green" ])
      <> if selected then [] else [ HH.ClassName "invisible" ]

-- Provided an array of items and any additional HTML, renders the container
-- Items should have already been treated with `boldMatches` by this point.
itemContainer
  :: ∀ p action
   . Maybe Int
  -> Array HH.PlainHTML
  -> Array (HH.HTML p (Select.Action action))
  -> HH.HTML p (Select.Action action)
itemContainer highlightIndex itemsHTML addlHTML =
  HH.div
    ( Setters.setContainerProps [ HP.classes itemContainerClasses ] )
    ( renderItems <> addlHTML )
  where
    hover :: Int -> Array HH.ClassName
    hover i = if highlightIndex == Just i then HH.ClassName <$> [ "bg-grey-lighter" ] else mempty

    renderItems :: Array (HH.HTML p (Select.Action action))
    renderItems =
      [ HH.ul
        [ HP.classes ulClasses ]
        $ mapWithIndex
          ( \i h ->
              HH.li
                ( Setters.setItemProps i
                  [ HP.classes $ liClasses <> hover i ]
                )
                [ HH.fromPlainHTML h ]
          )
          itemsHTML
      ]

-- Provided an array of selection items, renders them in a container
-- Make sure the array of items includes the correct click handlers
selectionContainer :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
selectionContainer []   =
  HH.div_ []
selectionContainer html =
  HH.div
  [ HP.classes selectionContainerClasses ]
  [ HH.ul
    [ HP.classes ulClasses ]
    $ html <#>
    ( \h ->
        HH.li
          [ HP.classes (HH.ClassName "py-2" : liClasses) ]
          [ h ]
    )
  ]

sectionClasses :: Array HH.ClassName
sectionClasses = HH.ClassName <$>
  [ "my-8"
  ]

  button :: Int -> HH.HTML p i
  button btn =
    HH.button
    (if btn == skip then disabled else active)
    [ HH.span_
      [ HH.text $ show btn ]
    ]
    where
    disabled =
      [ Ocelot.HTML.Properties.css "border border-black disabled focus:outline-none inline-block text-center"
      , Ocelot.HTML.Properties.style <<< Foreign.Object.fromHomogeneous
        $ { "border-radius": "50%"
          , "width": "2.5rem"
          , "line-height": "2.5rem"
          }
      ]

    active =
      [ Ocelot.HTML.Properties.css "inline-block hover:bg-grey hover:text-white text-center"
      , Ocelot.HTML.Properties.style <<< Foreign.Object.fromHomogeneous
        $ { "border-radius": "50%"
          , "width": "2.5rem"
          , "line-height": "2.5rem"
          }
      , HE.onMouseDown $ query btn
      ]

  ellipsis :: HH.HTML p i
  ellipsis =
    HH.span
    [ Ocelot.HTML.Properties.css "mx-2"]
    [ HH.text "…" ]

bar :: ∀ p i. Ratio Number -> Array (HH.IProp HTMLdiv i) -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
bar r divProps spanProps =
  HH.div
    ( [ css "bg-grey-light relative" ] <&> divProps )
    [ HH.span
      ( [ css "block", HP.attr (HH.AttrName "style") ("width: " <> value <> "%; text-indent: -9999px;") ] <&> spanProps )
      [ HH.text $ "Progress: " <> value ]
    ]
  where
    value :: String
    value = show $ if percentage > 100.0 then 100.0 else percentage

    percentage :: Number
    percentage = numerator r / denominator r * 100.0

-- Defaults
bar_ :: ∀ p i. Ratio Number -> HH.HTML p i
bar_ r = bar r [] [ css "bg-blue" ]


singleTab
  :: ∀ p i page
   . Eq page
  => page
  -> Tab i page
  -> HH.HTML p i
singleTab activePage tab =
  HH.li
    [ HP.class_ $ HH.ClassName "mr-12" ]
    [ HH.a
      ( tab.props
        <> [ HP.classes $ tabClasses <> conditionalTabClasses isActive ]
      )
      ( errorIcon
        <>
        [ HH.span
          [ HP.classes tabTextClasses ]
          [ HH.text tab.name ]
        ]
      )
    ]
  where


pullVisibleProp
  :: ∀ r i
   . Array (HH.IProp ( visible :: Boolean | r ) i)
  -> Tuple Boolean (Array (HH.IProp r i))
pullVisibleProp = foldr f (Tuple false [])
  where
    f (HP.IProp (HC.Property "visible" x)) =
      lmap $ const $ coerceExpanded x
    f iprop = rmap $ (flip snoc) $ coerceR iprop

    coerceExpanded :: HC.PropValue -> Boolean
    coerceExpanded = unsafeCoerce

    coerceR :: HH.IProp ( visible :: Boolean | r ) i -> HH.IProp r i
    coerceR = unsafeCoerce


tooltipClasses :: Array HH.ClassName
tooltipClasses = HH.ClassName <$> classesArr <> [ "-my-8" ]

trayOpenClasses :: Array HH.ClassName
trayOpenClasses = HH.ClassName <$>
  [ "pin-b" ]

trayClosedClasses :: Array HH.ClassName
trayClosedClasses = HH.ClassName <$>
  [ "-pin-b-40" ]

calendarHeader :: forall m. CompositeComponentHTML m
calendarHeader =
  HH.div
    [ css "flex text-grey-70" ]
    ( headers <#>
      \day ->
        HH.div
          [ css "w-14 h-14 flex items-center justify-center" ]
          [ HH.text day ]
    )
  where
    headers = [ "S", "M", "T", "W", "T", "F", "S" ]

    arrowButton q =
      Button.buttonClear
        [ HE.onClick $ S.Action <<< const q
        , css "text-grey-70 p-3"
        ]

    -- Show the month and year
    dateHeader =
      HH.div
        [ css "flex-1" ]
        [ HH.text monthYear ]


renderCalendar :: forall m. Year -> Month -> Array CalendarItem -> CompositeComponentHTML m
renderCalendar y m calendarItems =
  Layout.popover
    ( SS.setContainerProps
      [ HP.classes dropdownClasses ]
    )
    [ calendarNav y m
    , calendarHeader
    , HH.div_ $ renderRows $ rowsFromArray calendarItems
    ]
  where
    dropdownClasses :: Array HH.ClassName
    dropdownClasses = HH.ClassName <$>
      [ "pin-t"
      , "pin-l"
      , "p-6"
      , "bg-white"
      , "text-center"
      , "text-lg"
      ]

renderItem :: forall m. Int -> CalendarItem -> CompositeComponentHTML m
renderItem index item =
  HH.div
  -- Here's the place to use info from the item to render it in different
  -- states.
  -- if highlightedIndex == Just index then 'highlight' else 'dont'
  -- Because there are so many possible states, what about a helper like
  -- getCalendarStyles?
    ( maybeSetItemProps index item
      [ css
        $ trim
        $ "w-14 h-14 rounded-full relative "
          <> "flex items-center justify-center "
          <> "transition-1/4 border border-white "
          <> "before:no-content before:transition-1/4 "
          <> "before:w-full before:h-full "
          <> "before:absolute before:pin-t before:pin-l "
          <> (getCalendarStyles item)
      ]
    )
    -- printDay will format our item correctly
    [ HH.span
      [ css (getLabelStyles item) ]
      [ HH.text $ printDay item ]
    ]
  where
    -- If the calendar item is selectable,
    -- then augment the props with the correct click events.
    -- if not, then just don't provide the props at all.
    -- this is an easy way to "disable" functionality in the calendar.
    maybeSetItemProps i (CalendarItem Selectable _ _ _) props =
      SS.setItemProps i props
    maybeSetItemProps _ _ props = props

    -- Get the correct styles for a calendar item, dependent on its statuses
    getCalendarStyles :: CalendarItem -> String
    getCalendarStyles i
      = trim $ getSelectableStyles i
      <> " " <> getSelectedStyles i
      <> " " <> getBoundaryStyles i
      where
        getSelectableStyles :: CalendarItem -> String
        getSelectableStyles (CalendarItem NotSelectable _ _ _) =
          ""
        getSelectableStyles _ =
          "cursor-pointer hover:border hover:border-blue-88"

        getSelectedStyles :: CalendarItem -> String
        getSelectedStyles (CalendarItem _ Selected _ _) =
          "bg-blue-88 text-white before:scale-1"
        getSelectedStyles _ =
          "before:scale-0"

        getBoundaryStyles :: CalendarItem -> String
        getBoundaryStyles (CalendarItem _ _ OutOfBounds _) =
          "text-grey-90"
        getBoundaryStyles _ = mempty

    getLabelStyles :: CalendarItem -> String
    getLabelStyles = case _ of
      CalendarItem NotSelectable _ InBounds _ ->
        "border-black strike-through"
      CalendarItem NotSelectable _ OutOfBounds _ ->
        "border-grey-90 strike-through"
      _ -> ""

render :: forall m. MonadAff m => ComponentRender m
render state =
  HH.div
    [ css "flex" ]
    [ HH.div
      [ css "w-1/2 mr-2" ]
      [ HH.slot _datepicker unit DatePicker.component
        { disabled: state.disabled
        , interval: do
            interval <- state.interval
            pure
              { start: interval.start <#> Date.DateTime.date
              , end: interval.end <#> Date.DateTime.date
              }
        , selection: state.date
        , targetDate: state.targetDate
        }
        HandleDate
      ]
    , HH.div
      [ css "flex-1" ]
      [ HH.slot _timepicker unit TimePicker.component
        { disabled: state.disabled
        , interval: do
            interval <- state.interval
            pure
              { start:
                  if (interval.start <#> Date.DateTime.date) == state.date then
                    interval.start <#> Date.DateTime.time
                  else
                    Nothing
              , end:
                  if (interval.end <#> Date.DateTime.date) == state.date then
                    interval.end <#> Date.DateTime.time
                  else
                    Nothing
              }
        , selection: state.time
        }
        HandleTime
      ]
    ]


    ipropIdle :: Array (Halogen.HTML.Properties.IProp HTMLdiv Action)
ipropIdle =
  [ Ocelot.HTMl.Properties.css cssIdle
  , Ocelot.HTMl.Properties.style styleIdle
  , Ocelot.HTML.Events.onDrag (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragEnter DragEnter
  , Ocelot.HTML.Events.onDragOver (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragStart (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  ]
  where
  cssIdle :: String
  cssIdle = "bg-grey-70-a40 p-10"

  styleIdle :: Foreign.Object.Object String
  styleIdle =
    Foreign.Object.fromHomogeneous
      { outline: "2px dashed #8f9eb3"
      , "outline-offset": "-10px"
      , transition: "outline-offset .15s ease-in-out, background-color .15s linear"
      }

ipropDragOver :: Array (Halogen.HTML.Properties.IProp HTMLdiv Action)
ipropDragOver =
  [ Ocelot.HTMl.Properties.css cssDragOver
  , Ocelot.HTMl.Properties.style styleDragOver
  , Ocelot.HTML.Events.onDragEnd (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragLeave DragLeave
  , Ocelot.HTML.Events.onDragOver (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDrop DropFile
  ]
  where
  cssDragOver :: String
  cssDragOver = "bg-grey-95 p-10"

  styleDragOver :: Foreign.Object.Object String
  styleDragOver =
    Foreign.Object.fromHomogeneous
    { outline: "2px dashed #8f9eb3"
    , "outline-offset": "-20px"
    , transition: "outline-offset .15s ease-in-out, background-color .15s linear"
    }

  renderIcon ::
  forall m.
  ComponentHTML m
renderIcon =
  Halogen.HTML.div
    [ Ocelot.HTMl.Properties.css "text-grey-50 text-center w-full" ]
    [ Ocelot.Block.Icon.download
      [ Ocelot.HTMl.Properties.css "text-5xl" ]
    ]

classify
  :: String
  -> String
classify str
  | startsWith "p" str && not null (classifySide $ drop 1 str)
    = "padding" <-> classifySide (drop 1 str)
  | startsWith "m" str && not null (classifySide $ drop 1 str)
    = "margin" <-> classifySide (drop 1 str)
  | startsWith "-m" str && not null (classifySide $ drop 2 str)
    = "margin" <-> classifySide (drop 2 str)
  | startsWith "min-" str = "min" <-> classify (drop 4 str)
  | startsWith "max-" str = "max" <-> classify (drop 4 str)
  | startsWith "w-" str = "width"
  | startsWith "h-" str = "height"
  | startsWith "overflow-" str && (classifyOverflow $ drop 9 str) /= drop 9 str
    = "overflow" <-> (classifyOverflow $ drop 9 str)
  | otherwise = str

classifySide
  :: String
  -> String
classifySide str
  | startsWith "t-" str = "top"
  | startsWith "r-" str = "right"
  | startsWith "b-" str = "bottom"
  | startsWith "l-" str = "left"
  | startsWith "x-" str = "horizontal"
  | startsWith "y-" str = "vertical"
  | startsWith "-" str = "all"
  | otherwise = ""

classifyOverflow
  :: String
  -> String
classifyOverflow str
  | startsWith "x-" str = "horizontal" <-> (classifyOverflow $ drop 2 str)
  | startsWith "y-" str = "vertical" <-> (classifyOverflow $ drop 2 str)
  | elem str ["auto", "hidden", "visible", "scroll"] = ""
  | otherwise = str


renderItem :: forall m. Int -> TimeUnit -> CompositeComponentHTML m
renderItem index item =
  HH.div
  -- Here's the place to use info from the item to render it in different
  -- states.
  -- if highlightedIndex == Just index then 'highlight' else 'dont'
    ( maybeSetItemProps index item
      [ css
        $ trim
        $ "relative p-3 transition-1/4 "
          <> (getTimeStyles item)
      ]
    )
    -- printDay will format our item correctly
    [ HH.text $ printTime item ]
  where
    -- If the timeunit is selectable,
    -- then augment the props with the correct click events.
    -- if not, then just don't provide the props at all.
    -- this is an easy way to "disable" functionality in the calendar.
    maybeSetItemProps i (TimeUnit Selectable _ _) props =
      Setters.setItemProps i props
    maybeSetItemProps _ _ props = props

    -- Get the correct styles for a time unit, dependent on its statuses
    getTimeStyles :: TimeUnit -> String
    getTimeStyles i
      = trim $ getSelectableStyles i
      <> " " <> getSelectedStyles i
      where
        getSelectableStyles :: TimeUnit -> String
        getSelectableStyles (TimeUnit NotSelectable _ _) =
          mempty
        getSelectableStyles _ =
          "cursor-pointer hover:bg-grey-97"

        getSelectedStyles :: TimeUnit -> String
        getSelectedStyles (TimeUnit _ Selected _) =
          "text-blue-88"
        getSelectedStyles _ =
          mempty

  inputProps
  :: ∀ action f item m
   . Boolean
  -> Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action f item m))
  -> Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action f item m))
inputProps disabled iprops = if disabled
  then iprops'
  else Select.Setters.setInputProps iprops'
  where
    iprops' = 
      [ Halogen.HTML.Properties.disabled disabled
      , Halogen.HTML.Properties.autocomplete false
      , Ocelot.HTML.Properties.css "focus:next:text-blue-88" 
      ] 
      <&> iprops

isDisabled :: ∀ i. Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput i) -> Boolean
isDisabled = Data.Array.foldr f false
  where
    f (Halogen.HTML.Properties.IProp (Halogen.HTML.Core.Property "disabled" disabled)) 
      | coercePropValue disabled == true = (||) true
    f _ = (||) false

    coercePropValue :: Halogen.HTML.Core.PropValue -> Boolean
    coercePropValue = Unsafe.Coerce.unsafeCoerce

linkClasses :: Boolean -> Array Halogen.HTML.ClassName
linkClasses = if _
  then Halogen.HTML.ClassName <$> [ "text-grey-70", "no-underline", "font-medium" ]
  else Ocelot.Block.Format.linkClasses

  renderError :: ∀ p i. Boolean -> Halogen.HTML.HTML p i
renderError error =
  Ocelot.Block.Conditional.conditional error
    [ Ocelot.HTML.Properties.css "flex items-center mt-1" ]
    [ Ocelot.Block.Icon.error
      [ Ocelot.HTML.Properties.css "text-2xl text-yellow" ]
    , Halogen.HTML.p
      [ Ocelot.HTML.Properties.css "ml-3 text-grey-50 font-light" ]
      [ Halogen.HTML.text "Some data could not be retrieved here." ]
    ]

renderHeaderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> String
  -> (item -> Halogen.HTML.PlainHTML)
  -> (Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderHeaderSearchDropdown defaultLabel resetLabel renderItem renderFuzzy st =
  renderSearchDropdown resetLabel label renderFuzzy st
  where
    label = Halogen.HTML.span
      [ Ocelot.HTML.Properties.css "text-white text-3xl font-thin cursor-pointer whitespace-no-wrap" ]
      [ Data.Maybe.maybe (Halogen.HTML.text defaultLabel) (Halogen.HTML.fromPlainHTML <<< renderItem) st.selected
      , Ocelot.Block.Icon.collapse [ Ocelot.HTML.Properties.css "ml-3 text-xl text-grey-50 align-middle" ]
      ]

renderMulti
  :: ∀ action item m
  . Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Array item m))
  -> (item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Array item m
  -> CompositeComponentRender action Array item m
renderMulti iprops renderItem renderContainer st =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "relative" ]
    [ if (not disabled && not Data.Array.null st.selected)
        then
          Halogen.HTML.a
            [ Ocelot.HTML.Properties.css "absolute -mt-7 pin-r underline text-grey-70 cursor-pointer"
            , Halogen.HTML.Events.onClick \_ -> Select.Action $ RemoveAll
            ]
            [ Halogen.HTML.text "Remove All" ]
        else
          Halogen.HTML.text ""
    , Ocelot.Block.ItemContainer.selectionContainer $ st.selected <#>
        if disabled
          then
            Halogen.HTML.fromPlainHTML <<< renderItem
          else
            \selected ->
              Ocelot.Block.ItemContainer.selectionGroup
                renderItem
                []
                [ Halogen.HTML.Events.onClick \_ -> Select.Action $ Remove selected ]
                selected
    , Ocelot.Block.Input.inputGroup_
      [ Ocelot.Block.Input.inputCenter $ inputProps disabled iprops
      , Ocelot.Block.Input.addonLeft_
        [ Ocelot.Block.Icon.search_ ]
      , Ocelot.Block.Input.addonCenter
        [ Ocelot.HTML.Properties.css $ if Network.RemoteData.isLoading st.items then "" else "offscreen" ]
        [ spinner ]
      , Ocelot.Block.Input.borderRight
        [ Halogen.HTML.Properties.classes $ linkClasses disabled ]
        [ Halogen.HTML.text "Browse" ]
      ]
    , Ocelot.Block.Conditional.conditional (st.visibility == Select.On)
        [ Ocelot.HTML.Properties.css "relative block" ]
        [ renderContainer st ]
    , renderError $ Network.RemoteData.isFailure st.items
    ]
  where
  disabled = st.disabled

renderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> Halogen.HTML.PlainHTML
  -> (Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderSearchDropdown resetLabel label renderFuzzy st =
  Halogen.HTML.label
    [ Ocelot.HTML.Properties.css "relative" ]
    [ Halogen.HTML.fromPlainHTML label
    , Halogen.HTML.div
      [ Halogen.HTML.Properties.classes
        $ Halogen.HTML.ClassName "min-w-80" :
          if st.visibility == Select.Off
            then [ Halogen.HTML.ClassName "offscreen" ]
            else []
      ]
      [ Ocelot.Block.ItemContainer.dropdownContainer
        [ renderInput, renderReset ]
        renderFuzzy
        ((==) st.selected <<< Just <<< _.original <<< Data.Newtype.unwrap)
        st.fuzzyItems
        st.highlightedIndex
      ]
    ]
  where
  renderInput =
    Halogen.HTML.div
      [ Ocelot.HTML.Properties.css "m-4 border-b-2 border-blue-88 pb-2 flex" ]
      [ Ocelot.Block.Icon.search [ Ocelot.HTML.Properties.css "mr-4 text-xl text-grey-70" ]
      , Halogen.HTML.input
        $ inputProps false [ Ocelot.HTML.Properties.css "no-outline w-full", Halogen.HTML.Properties.placeholder "Search" ]
      ]

  renderReset =
    Ocelot.Block.ItemContainer.dropdownItem
      Halogen.HTML.div
      [ Halogen.HTML.Events.onClick \_ -> Select.Action $ RemoveAll
      ]
      [ Halogen.HTML.text resetLabel ]
      ( Data.Maybe.isNothing st.selected )
      false