// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The main component
//
open Prelude
open OIDCAuth

module MonocleNav = {
  @react.component
  let make = (~active: string, ~store: Store.t) => {
    let (state, _) = store

    let navItem = (name, dest) => {
      let query = [
        switch state.query {
        | "" => list{}
        | q => list{"q=" ++ q}
        },
        switch dest {
        | "/changes" =>
          switch state.order {
          | None => list{}
          | Some(_) => list{"o=" ++ state.order->orderToQS}
          }
        | _ => list{}
        },
      ]->Belt.List.concatMany
      let navUrl =
        "/" ++
        state.index ++
        dest ++
        switch query {
        | list{} => ""
        | _ => "?" ++ query->concatSep("&")
        }
      <NavItem
        key={name}
        onClick={_ => navUrl->RescriptReactRouter.push}
        isActive={active == dest}
        preventDefault={true}
        _to={navUrl}>
        {name->str}
      </NavItem>
    }

    let navGroup = (title, xs) =>
      <NavGroup title key={title}>
        {xs->Belt.List.map(((a, b)) => navItem(a, b))->Belt.List.toArray->React.array}
      </NavGroup>

    <Nav>
      <NavList>
        {[
          navItem("Activity", "/"),
          navGroup("Changes", list{("Review Board", "/board"), ("Browse Changes", "/changes")}),
          navGroup("Projects", list{("Projects", "/projects"), ("Repositories", "/repos")}),
          navGroup(
            "People",
            list{
              ("Search authors", "/search_author"),
              ("Active authors", "/active_authors"),
              ("Peers strength", "/peers_strength"),
              ("New contributors", "/new_authors"),
              ("Groups", "/user_groups"),
            },
          ),
        ]->React.array}
      </NavList>
    </Nav>
  }
}

module Login = {
  module Validator = {
    @react.component
    let make = (~store: Store.t, ~usernameValidated: string, ~setShowLoginModal) => {
      let (_, dispatch) = store
      let get = {() => WebApi.Login.loginValidation({username: usernameValidated})}
      usernameValidated->String.length == 0
        ? React.null
        : {
            switch useAutoGetOn(get, usernameValidated) {
            | NotAsked => React.null
            | Loading(_) => <Spinner />
            | Failure(_) => <Alert variant=#Danger title={"Unable to verify author identity :("} />
            | Loaded(resp) =>
              switch (resp: LoginTypes.login_validation_response) {
              | LoginTypes.Validation_result(LoginTypes.Unknown_ident) =>
                <Alert variant=#Warning title={"Author identity not found"} />
              | LoginTypes.Validation_result(LoginTypes.Known_ident) => {
                  usernameValidated->Login->dispatch
                  setShowLoginModal(_ => false)
                  React.null
                }
              }
            }
          }
    }
  }
  module Modal = {
    @react.component
    let make = (~store: Store.t, ~setShowLoginModal) => {
      let loginTitle = "Login on Monocle"
      let (username, setUsername) = React.useState(_ => "")
      let (usernameValidated, setUsernameValidated) = React.useState(_ => "")
      let onChange = (value, _) => {
        setUsername(_ => value)
      }
      let onClick = e => {
        e->ReactEvent.Mouse.preventDefault
        setUsernameValidated(_ => username)
      }
      let close = e => {
        e->ReactEvent.Mouse.preventDefault
        setShowLoginModal(_ => false)
      }

      <LoginPage loginTitle>
        <Alert
          variant=#Info
          title={"This is not an authenticated login - Set your username to an author identity to get personalized content."}
        />
        <Form>
          <FormGroup label={"Username"} fieldId={"login"}>
            <TextInput id={"login"} onChange value={username} />
            <Validator store usernameValidated setShowLoginModal />
          </FormGroup>
          <ActionGroup>
            <Button _type=#Submit variant=#Primary onClick> {"Login"} </Button>
            <Button variant=#Primary onClick=close> {"Close"} </Button>
          </ActionGroup>
        </Form>
      </LoginPage>
    }
  }
  module Button = {
    @react.component
    let make = (~store: Store.t, ~setShowLoginModal) => {
      let (state, dispatch) = store
      let onClickLogin = _ => setShowLoginModal(_ => true)
      let onClickLogout = _ => Logout->dispatch
      <div style={ReactDOM.Style.make(~paddingRight="13px", ())}>
        {switch state.username {
        | Some(username) =>
          <Patternfly.Layout.Flex>
            <Patternfly.Layout.FlexItem>
              <Button
                variant=#Tertiary
                icon={<Patternfly.Icons.User color="cyan" title={username} />}
                onClick={_ => {
                  let homeUrl =
                    "/" ++
                    state.index ++
                    "/" ++
                    "author" ++
                    "/" ++
                    username->Js.Global.encodeURIComponent
                  homeUrl->RescriptReactRouter.push
                }}>
                {username}
              </Button>
            </Patternfly.Layout.FlexItem>
            <Patternfly.Layout.FlexItem>
              <div onClick={onClickLogout} style={ReactDOM.Style.make(~cursor="pointer", ())}>
                <Patternfly.Icons.Arrow color="coral" title="Logout" />
              </div>
            </Patternfly.Layout.FlexItem>
          </Patternfly.Layout.Flex>
        | None => <Button variant=#Tertiary onClick=onClickLogin> {"Login"} </Button>
        }}
      </div>
    }
  }
}

let logoPath = "/logo.png"

module About = {
  let buildLink = (url, name) =>
    <a href={url} target="_blank" rel="noopener noreferrer"> {name->str} </a>
  let defaultLink = buildLink("https://changemetrics.io", "Learn more about Monocle")
  let buildLinkItems = (links: list<Web.ConfigTypes.about_about_link>) =>
    links
    ->Belt.List.map(l =>
      <TextListItem key={l.url} component=#Dt> {buildLink(l.url, l.name)} </TextListItem>
    )
    ->Belt.List.toArray
    ->React.array
  let buildCategoryLinks = (links: list<Web.ConfigTypes.about_about_link>) =>
    links
    ->Belt.List.map(l => l.category)
    ->Belt.List.reduce(list{}, (acc, item) =>
      acc->Belt.List.has(item, (a, b) => a == b) ? acc : acc->Belt.List.add(item)
    )
    ->Belt.List.map(cat =>
      <TextContent key={cat}>
        <h4> {cat->str} </h4>
        <TextList component=#Dl>
          {links->Belt.List.keep(i => i.category == cat)->buildLinkItems}
        </TextList>
      </TextContent>
    )
    ->Belt.List.toArray
    ->React.array

  @react.component
  let make = (~store: Store.t, ~isOpen: bool, ~onClose: unit => unit) => {
    let (state, _) = store
    <AboutModal isOpen onClose productName="Monocle" brandImageAlt="Monocle" brandImageSrc=logoPath>
      <TextContent>
        <h4> {"About Monocle"->str} </h4>
        <TextList component=#Dl>
          <TextListItem component=#Dt> {"Monocle Version"->str} </TextListItem>
          <TextListItem component=#Dd> {state.about.version} </TextListItem>
          <TextListItem component=#Dt> {defaultLink} </TextListItem>
        </TextList>
      </TextContent>
      {state.about.links->buildCategoryLinks}
    </AboutModal>
  }
}

module App = {
  @react.component
  let make = (~about: ConfigTypes.about) => {
    let url = RescriptReactRouter.useUrl()

    let oidcCallbackUrl = readWindowLocationOrigin() ++ "/auth/callback"
    Js.log(oidcCallbackUrl)
    let manager = getUserManager(
      "https://accounts.google.com",
      "337026574341-fdtl4e1v4edhspriu9dqo8er3ha1mf3o.apps.googleusercontent.com",
      oidcCallbackUrl,
    )
    Js.log(manager)

    // let puV = getUser(manager)
    // Js.log(pu)

    // The initial index
    let initIndex = switch url.path->Belt.List.head->Belt.Option.getWithDefault("") {
    | "help" => ""
    | x => x
    }

    // The current nav
    let active = switch url.path {
    | list{} => ""
    | list{_, ...xs} => "/" ++ Js.Array.joinWith("/", xs->Belt.List.toArray)
    }

    // Init the APP store
    let store = Store.use(initIndex, about)
    let (state, dispatch) = store

    // Some state for managing various modals
    let (showAbout, setShowAbout) = React.useState(_ => false)
    let (showLoginModal, setShowLoginModal) = React.useState(_ => false)

    // The main page to render APP
    let bodyPage = {
      let nav = <MonocleNav active store />
      let sidebar = state.index == "" ? React.null : <PageSidebar nav />
      let logo = <span onClick={_ => store->Store.changeIndex("")}> <img src={logoPath} /> </span>
      let headerTools =
        <PageHeaderTools>
          <About store isOpen=showAbout onClose={() => setShowAbout(_ => false)} />
          <PageHeaderToolsGroup>
            <PageHeaderToolsItem> <Login.Button store setShowLoginModal /> </PageHeaderToolsItem>
            <PageHeaderToolsItem>
              <div
                onClick={_ => setShowAbout(_ => true)}
                style={ReactDOM.Style.make(~cursor="pointer", ())}>
                <Patternfly.Icons.InfoAlt />
              </div>
            </PageHeaderToolsItem>
            // <PageHeaderToolsItem>
            //   <div
            //     onClick={showSettings}
            //     style={ReactDOM.Style.make(~cursor="pointer", ~paddingLeft="13px", ())}>
            //     <Patternfly.Icons.Cog />
            //   </div>
            // </PageHeaderToolsItem>
          </PageHeaderToolsGroup>
        </PageHeaderTools>
      let header = <PageHeader showNavToggle={state.index == "" ? false : true} logo headerTools />
      <Page header sidebar isManagedSidebar={true}>
        {switch showLoginModal {
        | true => <Login.Modal store setShowLoginModal />
        | false =>
          <React.Fragment>
            {switch state.index {
            | "" => React.null

            | _ => <PageSection variant=#Dark> <Search.Top store /> </PageSection>
            }}
            <PageSection isFilled={true}>
              {switch url.path {
              | list{} => <Indices.Indices store />
              | list{"help", "search"} => <HelpSearch.View store />
              | list{_, "settings"} => <LocalSettings.View store />
              | list{_} => <Activity store />
              | list{_, "author", name} => <ScopedView.AuthorScopedView store name />
              | list{_, "group", name} => <ScopedView.GroupScopedView store name />
              | list{_, "active_authors"} => <ActivePeopleView store />
              | list{_, "peers_strength"} => <PeersStrengthView store stacked={false} />
              | list{_, "new_authors"} => <NewContributorsView store />
              | list{_, "projects"} => <ProjectsView store />
              | list{_, "user_groups"} => <GroupsView store />
              | list{_, "user_groups", group} => <GroupView group store />
              | list{_, "repos"} => <ReposView store />
              | list{_, "changes"} => <NChangeView store />
              | list{_, "change", change} => <ChangeView change store />
              | list{_, "board"} => <Board store />
              | list{_, "search_author"} => <AuthorSearch store />
              | list{"auth", "login"} => {
                  Js.log("Performing login")
                  let p = login(manager)
                  Js.log(p)
                  React.null
                }
              | list{"auth", "callback"} => {
                  Js.log("In Auth callback")
                  let p = signinRedirect(manager)
                  Js.log(p)
                  React.null
                }
              | _ => <p> {"Not found"->str} </p>
              }}
            </PageSection>
          </React.Fragment>
        }}
      </Page>
    }

    let toaster =
      <ul className="pf-c-alert-group pf-m-toast">
        {state.toasts
        ->Belt.List.mapWithIndex((idx, toast) =>
          <li className="pf-c-alert-group__item">
            <Patternfly.Alert
              key={idx->string_of_int}
              title={toast}
              timeout={4000}
              onTimeout={_ => toast->RemoveToast->dispatch}
            />
          </li>
        )
        ->Belt.List.toArray
        ->React.array}
      </ul>

    <React.Fragment> bodyPage toaster </React.Fragment>
  }
  // let showSettings = _ => "settings"->RescriptReactRouter.push
}

@react.component
let make = () =>
  <NetworkRender
    get={() => WebApi.Config.getAbout({void: ""})}
    trigger={""}
    render={(resp: ConfigTypes.get_about_response) => {
      switch resp.about {
      | Some(about) => <App about />
      | None => <Alert variant=#Danger title={"Unable to fetch about data !"} />
      }
    }}
  />

let default = make
