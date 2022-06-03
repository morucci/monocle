// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A library for monocle web
//

// Missing bindings from re-patternfly
include Patternfly // 'include' export a module value to open scope
open WebApi

// axiosGetCallback<string> is a function that goes from unit to axios<string>
type axiosGetCallback<'data> = unit => axios<'data>

// See https://rescript-lang.org/docs/manual/latest/interop-cheatsheet
@val @scope(("window", "location"))
external windowLocationSearch: string = "search"
let readWindowLocationSearch = () => windowLocationSearch

@val @scope(("window", "location"))
external windowLocationPathname: string = "pathname"
let readWindowLocationPathname = () => windowLocationPathname

@val @scope(("window", "location"))
external windowLocationOrigin: string = "origin"
let readWindowLocationOrigin = () => windowLocationOrigin

// Bindings for moment
%%raw(`
import moment from 'moment'
`)
let momentFromNow = %raw(`
  function(d) {
    return moment(d).fromNow()
  }
`)

let momentHumanizeDuration = %raw(`
  function(d) {
    return moment.duration(d, 'seconds').humanize()
  }
`)

let resetLocationSearch = %raw(`
  function() {
    window.history.replaceState(null, null, window.location.origin + window.location.pathname);
  }
`)

let setLocationSearch: (string, string) => unit = %raw(`
  function(q, v) {
    const url = new URL(window.location.href);
    url.searchParams.set(q, v);
    window.history.replaceState(null, null, url);
  }
`)

// Bindings for react router
module Link = {
  @react.component @module("react-router-dom")
  external make: (
    ~_to: string,
    ~children: 'children,
    ~style: option<'style>=?,
    ~target: option<string>=?,
    ~onClick: option<'cb>=?,
  ) => React.element = "Link"
}

// A temporary module to provide runtime setting
module Env = {
  type t = {
    @as("TD")
    td: string,
  }
  type react_env = {
    @as("REACT_APP_TD")
    td: option<string>,
  }
  @val external env: t = "window"
  @val @scope("process") external react_env: react_env = "env"
}

module Time = {
  type t

  let getNow = Js.Date.make

  let getSimpleDate = (d: Js.Date.t): string =>
    Js.String.split("T", d->Js.Date.toISOString)->Belt.Array.getUnsafe(0)

  let getDateMinusYear = (delta: int): string => {
    let now = getNow()
    let curYear = now->Js.Date.getUTCFullYear->Belt.Float.toInt
    let newYear = (curYear - delta)->Belt.Float.fromInt
    // Mutate the date
    Js.Date.setUTCFullYear(now, newYear)->ignore
    now->getSimpleDate
  }

  let getDateMinusMonth = (delta: int): string => {
    let now = getNow()
    let curMonth = now->Js.Date.getUTCMonth->Belt.Float.toInt
    let newMonth = (curMonth - delta)->Belt.Float.fromInt
    // Mutate the date
    Js.Date.setUTCMonth(now, newMonth)->ignore
    now->getSimpleDate
  }

  let getDateMinusWeek = (delta: int): string => {
    let now = getNow()
    let curHour = now->Js.Date.getUTCHours->Belt.Float.toInt
    let newHour = (curHour - delta * 24 * 7)->Belt.Float.fromInt
    // Mutate the date
    Js.Date.setUTCHours(now, newHour)->ignore
    now->getSimpleDate
  }
}

module URLSearchParams = {
  // https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams
  type t

  @new external make: string => t = "URLSearchParams"
  @send external toString: t => string = "toString"
  @send external get: (t, string) => Js.Nullable.t<string> = "get"
  @send external set: (t, string, string) => unit = "set"
  @send external delete: (t, string) => unit = "delete"

  let current = () => windowLocationSearch->make
}

// Table helpers
let usePagination = (rows: array<'a>): (array<'a>, React.element) => {
  let paginationThreshold = 20
  let (page, setPage) = React.useState(_ => 1)
  let (perPage, setPerPage) = React.useState(_ => paginationThreshold)
  let onSetPage = (_, pageNumber: int, _, _, _) => {
    setPage(_ => pageNumber)
  }
  let onPerPageSelect = (_, perPage: int, _, _, _) => {
    setPerPage(_ => perPage)
  }
  let itemCount = Belt.Array.length(rows)
  itemCount > paginationThreshold
    ? (
        rows->Belt.Array.slice(~offset=(page - 1) * perPage, ~len=perPage),
        <Pagination itemCount perPage page onSetPage onPerPageSelect />,
      )
    : (rows, React.null)
}

// Network helpers

type remoteData<'data> = NotAsked | Loading(option<'data>) | Loaded('data) | Failure(string)

// useAutoGet perform the 'get' effect when the calling component is mounted.
let useAutoGetOn = (get: axiosGetCallback<'data>, trigger: string): remoteData<'data> => {
  let (state, set') = React.useState(_ => NotAsked)
  let set = x => set'(_ => x)->Js.Promise.resolve
  let handleBegin = switch state {
  | Loaded(data) => data->Some->Loading
  | _ => None->Loading
  }
  let handleErr = err => {
    Js.log(err)
    "Network error"->Failure->set
  }
  let handleOk = resp => resp.data->Loaded->set
  React.useEffect1(() => {
    set'(_ => handleBegin)
    (get() |> Js.Promise.then_(handleOk) |> Js.Promise.catch(handleErr))->ignore
    None
  }, [trigger])
  state
}
let useAutoGet = x => x->useAutoGetOn("")

module NetworkRender = {
  @react.component
  let make = (~get, ~trigger, ~render) =>
    switch useAutoGetOn(get, trigger) {
    | NotAsked => <Spinner />
    | Loading(respM) => <>
        <Spinner />
        {switch respM {
        | Some(resp) => render(resp)
        | None => React.null
        }}
      </>
    | Failure(title) => <Alert variant=#Danger title />
    | Loaded(resp) => render(resp)
    }
}

// Convenient functions

let getDate = (ts: option<TimestampTypes.timestamp>): Js.Date.t =>
  ts->Belt.Option.getExn->Belt.Option.getExn

let getCurrentTime = () => Js.Date.now()->Js.Date.fromFloat

let str = React.string
// Render component if the predicate is true
let maybeRender = (pred: bool, component: React.element): React.element =>
  pred ? component : React.null
// Hide if predicate is Some(true) (useful in case of optional parameter)
let maybeHide = (pred: option<bool>, component: React.element): React.element =>
  switch pred {
  | Some(true) => React.null
  | _ => component
  }
// Render component if the list is not empty
let maybeRenderArray = (xs: array<'a>, component) =>
  xs->Belt.Array.length > 0 ? component : React.null
let maybeRenderList = (xs: list<'a>, component) =>
  switch xs {
  | list{} => React.null
  | _ => component
  }
let lower = s => s->Js.String.toLowerCase
let int32_str = i32 => string_of_int(Int32.to_int(i32))
let float_str = Js.Float.toFixedWithPrecision(~digits=1)

let quoteValue = v => Js.String.includes(" ", v) ? "\"" ++ v ++ "\"" : v

// the take from haskell prelude
let rec take: (list<'a>, int) => list<'a> = (xs, count) =>
  switch xs {
  | list{x, ...tail} if count > 0 => tail->take(count - 1)->Belt.List.add(x)
  | _ => list{}
  }

// Check if a text list contains an element
let elemText = (xs: list<string>, x: string) => xs->Belt.List.has(x, (a, b) => a == b)
// Get an optional value with default
let fromMaybe = (maybe: option<'a>, default: 'a): 'a => maybe->Belt.Option.getWithDefault(default)
let mapWithKey = (xs, f) => xs->Belt.List.mapWithIndex((index, x) => f(string_of_int(index), x))
// Remove empty optional from a list
let rec catMaybes = (xs: list<option<'a>>): list<'a> =>
  switch xs {
  | list{} => list{}
  | list{None, ...rest} => catMaybes(rest)
  | list{Some(x), ...rest} => catMaybes(rest)->Belt.List.add(x)
  }
// Join a list of string with a separator
let rec prependToAll = (xs, sep): string =>
  switch xs {
  | list{} => ""
  | list{x, ...rest} => sep ++ x ++ rest->prependToAll(sep)
  }
let concatSep = (xs: list<string>, sep: string): string =>
  switch xs {
  | list{} => ""
  | list{x, ...rest} => x ++ rest->prependToAll(sep)
  }

let useToggle = default => {
  let (value, setValue) = React.useState(_ => default)
  let toggle = () => setValue(x => !x)
  let set = x => setValue(_ => x)
  (value, toggle, set)
}

// Combine two queries
let addQuery = (baseQuery, extraQuery) => {
  let isEmpty = s => s->Js.String.trim == ""
  let (query, sep) = baseQuery->isEmpty ? ("", "") : ("(" ++ baseQuery ++ ")", " and ")
  query ++ (extraQuery->isEmpty ? "" : sep ++ extraQuery)
}

// Decode/encode order
let orderFromQS: string => option<SearchTypes.order> = queryString =>
  switch Js.String.split("#", queryString) {
  | [field, "A"] => {field: field, direction: Asc}->Some
  | [field, "D"] => {field: field, direction: Desc}->Some
  | _ => None
  }

let orderDirToQS = (dir: SearchTypes.order_direction) =>
  switch dir {
  | Asc => "#A"
  | Desc => "#D"
  }

let orderToQS = (order: option<SearchTypes.order>) =>
  switch order {
  | None => ""
  | Some({field, direction}) => field ++ direction->orderDirToQS
  }

let orderDirToString = (dir: SearchTypes.order_direction) =>
  switch dir {
  | Asc => ""
  | Desc => " DESC"
  }

// Monocle style:
// an expandable panel
module MExpandablePanel = {
  @react.component
  let make = (~title, ~children, ~stateControler: option<(bool, (bool => bool) => unit)>=?) => {
    let (show, setShow) = switch stateControler {
    | Some(s, st) => (s, st)
    | None => React.useState(_ => false)
    }
    let toggleProps = {
      "id": "toggle-button",
      "aria-label": "Details",
      "aria-labelledby": "titleId toggle-button",
      "aria-expanded": !show,
      "icon": React.null,
    }
    <Card isCompact={true} isExpanded=show>
      <CardHeader onExpand={(_, _) => setShow(v => !v)} toggleButtonProps={toggleProps}>
        <CardTitle> {title->React.string} </CardTitle>
      </CardHeader>
      <CardExpandableContent> <CardBody> {children} </CardBody> </CardExpandableContent>
    </Card>
  }
}

// a grid with space between element
module MGrid = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.Grid hasGutter=true> {children} </Patternfly.Layout.Grid>
}

// a grid element that can fit 3 on medium screen
module MGridItem = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem md=Column._4> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl11 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._11> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl10 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._10> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl9 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._9> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl8 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._8> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl7 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._7> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl6 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._6> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl5 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._5> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl4 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._4> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl3 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._3> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl2 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._2> {children} </Patternfly.Layout.GridItem>
}

module MGridItemXl1 = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem xl=Column._1> {children} </Patternfly.Layout.GridItem>
}

module MStack = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.Stack hasGutter=true> {children} </Patternfly.Layout.Stack>
}

module MStackItem = {
  @react.component
  let make = (~children) => <Patternfly.Layout.StackItem> {children} </Patternfly.Layout.StackItem>
}

module MCenteredContent = {
  @react.component
  let make = (~children) =>
    <MStack>
      <MStackItem> {""->str} </MStackItem>
      <MStackItem>
        <MGrid>
          <Patternfly.Layout.GridItem xl=Column._1 />
          <Patternfly.Layout.GridItem xl=Column._10> {children} </Patternfly.Layout.GridItem>
          <Patternfly.Layout.GridItem xl=Column._1 />
        </MGrid>
      </MStackItem>
    </MStack>
}

module SortableTable = {
  @react.component
  let make = (
    ~items: list<'a>,
    ~defaultSortedColumn: int,
    ~columnNames: array<string>,
    ~isOrdered: ('a, 'a, int) => bool,
    ~formatters: list<'a => React.element>,
  ) => {
    let (rows, setRows) = React.useState(_ => [])
    let (sortBy, setSortBy) = React.useState(_ => {index: defaultSortedColumn, direction: #asc})
    let doSort = rows => rows->sortRows(isOrdered)
    let onSort = (_, index, direction) => {
      setRows(_ => doSort(rows, index, direction))
      setSortBy(_ => {index: index, direction: direction})
    }

    let columns = columnNames->Belt.Array.map(name => {title: name, transforms: [sortable]})

    React.useEffect1(() => {
      setRows(_ => items->mkRows(formatters)->doSort(sortBy.index, sortBy.direction))
      None
    }, [items])
    <Table caption=" " variant=#compact rows cells=columns sortBy onSort>
      <TableHeader /> <TableBody />
    </Table>
  }
}

module MSelect = {
  let maxCount = 20

  @react.component
  let make = (
    ~value: string,
    ~placeholder: string,
    ~multi: bool=true,
    ~isCreatable: bool=false,
    ~options,
    ~valueChanged,
  ) => {
    let (isOpen, _, onToggle) = useToggle(false)
    let selections = value != "" ? Js.String.split(",", value) : []
    let onSelect = (_, newValue, _) => {
      let nextValues = multi
        ? selections->Js.Array2.includes(newValue)
            ? selections->Js.Array2.filter(v => v != newValue)
            : selections->Js.Array2.concat([newValue])
        : [newValue]
      nextValues->Js.Array2.joinWith(",")->valueChanged
      onToggle(false)
    }
    let onClear = _ => {
      valueChanged("")
    }
    let onCreateOption = v => {
      selections->Js.Array2.concat([v])->Js.Array2.joinWith(",")->valueChanged
      onToggle(false)
    }
    let placeholderText = placeholder
    let variant = multi ? #Typeaheadmulti : #Single
    let inlineFilterPlaceholderText = value != "" ? placeholder ++ ": " ++ value : ""
    let onFilter = (_, currentInput) => {
      switch currentInput {
      | "" => options
      | _ => options->Belt.List.keep(opt => currentInput->lower->Js.String.startsWith(opt->lower))
      }
      ->take(maxCount)
      ->Belt.List.map(s => <SelectOption key={s} value={s} />)
      ->Belt.List.toArray
    }

    <Patternfly.Select
      variant
      placeholderText
      inlineFilterPlaceholderText
      selections
      isOpen
      onClear
      onSelect
      onCreateOption
      onFilter
      onToggle
      isCreatable>
      {options
      ->take(maxCount)
      ->mapWithKey((key, name) => <SelectOption key value={name} />)
      ->Belt.List.toArray
      ->React.array}
    </Patternfly.Select>
  }
}

let addProp = (props, valueM, mkProp) =>
  valueM
  ->Belt.Option.flatMap(prop => prop->mkProp->Some)
  ->Belt.Option.getWithDefault(Js.Obj.empty())
  ->Js.Obj.assign(props)

// a compact card without title
module MSimpleCard = {
  @react.component
  let make = (
    ~children,
    ~style: option<ReactDOM.Style.t>=?,
    ~onClick: option<ReactEvent.Mouse.t => unit>=?,
  ) => {
    let cardProps =
      Patternfly.Card.makeProps(~isCompact=true, ())
      ->addProp(onClick, prop => {"onClick": prop})
      ->addProp(style, prop => {"style": prop})
    React.createElementVariadic(
      Patternfly.Card.make,
      cardProps,
      [
        React.createElementVariadic(
          Patternfly.CardBody.make,
          Patternfly.CardBody.makeProps(),
          [children],
        ),
      ],
    )
  }
}

module LimitSelector = {
  @react.component
  let make = (~limit: int, ~setLimit: (int => int) => unit, ~default: int, ~values: list<int>) => {
    let setLimit' = str => {
      let v = str == "" ? default : str->int_of_string
      setLimit(_ => v)
    }
    <Tooltip content={"Select the amount of item to display"}>
      <MSelect
        placeholder={"Set limit"}
        options={values->Belt.List.map(string_of_int)}
        multi={false}
        value={limit > 0 ? limit->string_of_int : ""}
        valueChanged={setLimit'}
      />
    </Tooltip>
  }
}

let horizontalSpacing = ReactDOM.Style.make(~paddingLeft="5px", ~paddingRight="5px", ())

module ExternalLink = {
  @react.component
  let make = (~href, ~title) =>
    <a href target="_blank" rel="noopener noreferre" style={horizontalSpacing}>
      {(`🔗 ` ++ title)->str}
    </a>
}

module MonoCard = {
  @react.component
  let make = (
    ~title: string,
    ~tooltip_content: string,
    ~icon: React.element,
    ~limitSelector: option<React.element>=?,
    ~children,
  ) =>
    <Card isCompact=true>
      <CardTitle>
        <MGrid>
          <MGridItemXl9>
            <Title headingLevel=#H3>
              <Tooltip content=tooltip_content> {icon} </Tooltip> {(" " ++ title)->str}
            </Title>
          </MGridItemXl9>
          {limitSelector->Belt.Option.isSome
            ? <MGridItemXl3> {limitSelector} </MGridItemXl3>
            : React.null}
        </MGrid>
      </CardTitle>
      <CardBody> {children} </CardBody>
    </Card>
}

module QueryRender = {
  @react.component
  let make = (~request, ~trigger, ~render) =>
    <NetworkRender
      get={() => WebApi.Search.query(request)}
      trigger
      render={resp =>
        switch resp {
        | SearchTypes.Error(err) =>
          <Alert
            title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))}
            variant=#Danger
          />
        | resp => render(resp)
        }}
    />
}

// This structure could be used to harmonize the QueryRender and NetworkRender
module Renderer = {
  type t<'response, 'data> = {
    title: string,
    tooltipContent: string,
    matcher: 'response => option<'data>,
    card: 'data => React.element,
  }
}

module QueryRenderCard = {
  @react.component
  let make = (
    // The request to perform by the card
    ~request: Web.SearchTypes.query_request,
    // The string to watch for change to run the request
    ~trigger: string,
    // THe title of the card
    ~title: string,
    // The content of the tooltip displayed by the icon
    ~tooltip_content: string,
    // The icon
    ~icon: React.element,
    // An option limitSelector component
    ~limitSelector: option<React.element>=?,
    // A matcher function to extract data from the response
    ~match: Web.SearchTypes.query_response => option<'a>,
    // The child builder
    ~childrenBuilder: 'a => React.element,
  ) => {
    let render = resp => {
      switch match(resp) {
      | Some(data) =>
        <MonoCard title tooltip_content icon ?limitSelector> {childrenBuilder(data)} </MonoCard>
      | None => React.null
      }
    }
    <QueryRender request trigger render />
  }
}

// Minimal Patternfly Tabs component binding
// TODO: Remove after new release of re-patternfly
module MonoTabs = {
  @react.component @module("@patternfly/react-core")
  external make: (
    ~children: 'children,
    ~activeKey: string=?,
    ~isBox: bool=?,
    ~onSelect: (unit, string) => unit=?,
  ) => React.element = "Tabs"
}
