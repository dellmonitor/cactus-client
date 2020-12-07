import { Elm } from './Main.elm'

function initComments(config) {
  var {
    node,
    defaultHomeserverUrl,
    serverName,
    siteName,
    commentSectionId,
    pageSize = 10,
  } = config;

  // make a comments section in DOM element `node`
  // initialize with provided config, and nullable session from localstorage
  var app = Elm.Main.init({
    node: node,
    flags: {
      defaultHomeserverUrl: defaultHomeserverUrl,
      serverName: serverName,
      siteName: siteName,
      commentSectionId: commentSectionId,
      storedSession: JSON.parse(localStorage.getItem("cactus-session")),
      pageSize: pageSize
    }
  });

  // subscribe to commands from localstorage port
  app.ports.storeSession.subscribe(s => window.localStorage.setItem("cactus-session", s));
}

window.initComments = initComments;
