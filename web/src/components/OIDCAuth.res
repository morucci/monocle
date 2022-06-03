%%raw(`
import { UserManager, WebStorageStateStore } from "oidc-client-ts"
`)

type userManager = unit

let getUserManager: (string, string, string) => userManager = %raw(`
  function(authority, clientId, redirectUri) {
    const userManagerConfig = {
      // userStore: new WebStorageStateStore({ store: window.localStorage }),
      authority: authority,
      client_id: clientId,
      redirect_uri: redirectUri,
      // client_authentication: "none"
      // client_secret: ""
    }
    const mgr = new UserManager(userManagerConfig);
    return mgr
  }
`)

type user = unit

let getUser: userManager => Promise.t<user> = %raw(`
  function (userManager) {
    userManager.getUser()
      .then(user => console.log("user: " + user))
      .catch(err => console.log("fail: " + err))
  }
`)

let login = %raw(`
  function (userManager) {
    userManager.signinRedirect()
      .then(ret => console.log("ret: " + ret))
      .catch(err => console.log("fail: " + err))
  }
`)

let signinRedirect: userManager => Promise.t<user> = %raw(`
  function (userManager) {
    userManager.signinRedirectCallback()
      .then(user => console.log("user: " + user.id_token))
      .catch(err => console.log("fail: " + err))
  }
`)
