
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
            [ css "text-lg text-grey-70 mr-4" ] -- TODO text-grey-70
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
    anchorClass Left = "pin-r-full" -- TODO pin
    anchorClass Right = "pin-l-full" -- TODO pin
    anchorClass Top = "pin-b-full" -- TODO pin
    anchorClass Bottom = "pin-t-full" -- TODO pin


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
      <> ( if highlighted then [ HH.ClassName "bg-grey-lighter" ] else [] ) -- TODO
      <> if selected then [ HH.ClassName "font-medium" ] else []

    checkmarkClass :: Array HH.ClassName
    checkmarkClass =
      (HH.ClassName <$> [ "mr-2", "text-green" ]) -- TODO text-green
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
    hover i = if highlightIndex == Just i then HH.ClassName <$> [ "bg-grey-lighter" ] else mempty -- TODO 

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

  button :: Int -> HH.HTML p i
  button btn =
    HH.button
    (if btn == skip then disabled else active)
    [ HH.span_
      [ HH.text $ show btn ]
    ]
    active =
      [ Ocelot.HTML.Properties.css "inline-block hover:bg-grey hover:text-white text-center" -- TODO bg-grey
      , Ocelot.HTML.Properties.style <<< Foreign.Object.fromHomogeneous
        $ { "border-radius": "50%"
          , "width": "2.5rem"
          , "line-height": "2.5rem"
          }
      , HE.onMouseDown $ query btn
      ]

bar :: ∀ p i. Ratio Number -> Array (HH.IProp HTMLdiv i) -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
bar r divProps spanProps =
  HH.div
    ( [ css "bg-grey-light relative" ] <&> divProps ) -- TODO bg-grey-light
-- Defaults
bar_ :: ∀ p i. Ratio Number -> HH.HTML p i
bar_ r = bar r [] [ css "bg-blue" ] -- TODO bg-blue


trayOpenClasses :: Array HH.ClassName
trayOpenClasses = HH.ClassName <$>
  [ "pin-b" ]

trayClosedClasses :: Array HH.ClassName
trayClosedClasses = HH.ClassName <$>
  [ "-pin-b-40" ]

calendarHeader :: forall m. CompositeComponentHTML m
calendarHeader =
    arrowButton q =
      Button.buttonClear
        [ HE.onClick $ S.Action <<< const q
        , css "text-grey-70 p-3"
        ]


renderCalendar :: forall m. Year -> Month -> Array CalendarItem -> CompositeComponentHTML m
renderCalendar y m calendarItems =
  where
    dropdownClasses :: Array HH.ClassName
    dropdownClasses = HH.ClassName <$>
      [ "pin-t" -- TODO
      , "pin-l" -- TODO
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
          <> "before:absolute before:pin-t before:pin-l " -- TODO 
          <> (getCalendarStyles item)
      ]
    )
        getSelectableStyles :: CalendarItem -> String
        getSelectableStyles (CalendarItem NotSelectable _ _ _) =
          ""
        getSelectableStyles _ =
          "cursor-pointer hover:border hover:border-blue-88" -- TODO

        getSelectedStyles :: CalendarItem -> String
        getSelectedStyles (CalendarItem _ Selected _ _) =
          "bg-blue-88 text-white before:scale-1" -- TODO
        getSelectedStyles _ =
          "before:scale-0"

        getBoundaryStyles :: CalendarItem -> String
        getBoundaryStyles (CalendarItem _ _ OutOfBounds _) =
          "text-grey-90" -- TODO
        getBoundaryStyles _ = mempty

    getLabelStyles :: CalendarItem -> String
    getLabelStyles = case _ of
      CalendarItem NotSelectable _ InBounds _ ->
        "border-black strike-through" -- TODO 
      CalendarItem NotSelectable _ OutOfBounds _ ->
        "border-grey-90 strike-through" -- TODO
      _ -> ""


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
  cssIdle = "bg-grey-70-a40 p-10" -- TODO

  styleIdle :: Foreign.Object.Object String
  styleIdle =
    Foreign.Object.fromHomogeneous
      { outline: "2px dashed #8f9eb3" -- REVIEW is this just used as inline style? on SVG for example?
      , "outline-offset": "-10px"
      , transition: "outline-offset .15s ease-in-out, background-color .15s linear"
      }

  cssDragOver :: String
  cssDragOver = "bg-grey-95 p-10" -- TODO

  styleDragOver :: Foreign.Object.Object String
  styleDragOver =
    Foreign.Object.fromHomogeneous
    { outline: "2px dashed #8f9eb3"-- REVIEW is this just used as inline style? on SVG for example?
    , "outline-offset": "-20px"
    , transition: "outline-offset .15s ease-in-out, background-color .15s linear"
    }

  renderIcon ::
  forall m.
  ComponentHTML m
renderIcon =
  Halogen.HTML.div
    [ Ocelot.HTMl.Properties.css "text-grey-50 text-center w-full" ] -- TODO
    [ Ocelot.Block.Icon.download
      [ Ocelot.HTMl.Properties.css "text-5xl" ]
    ]




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
          "cursor-pointer hover:bg-grey-97" -- TODO

        getSelectedStyles :: TimeUnit -> String
        getSelectedStyles (TimeUnit _ Selected _) =
          "text-blue-88" -- TODO
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
      , Ocelot.HTML.Properties.css "focus:next:text-blue-88" -- TODO
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