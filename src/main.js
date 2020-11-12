import { Elm } from './Main.elm'

function initComments({node, defaultHomeserverUrl, siteName, uniqueId, serverName}) {
  // make a comments section in DOM element `node`
  // initialize with provided config, and nullable session from localstorage
  var app = Elm.Main.init({
    node: node,
    flags: {
      defaultHomeserverUrl: defaultHomeserverUrl,
      siteName: siteName,
      uniqueId: uniqueId,
      serverName: serverName,
      storedSession: localStorage.getItem("cactus-session") 
    }
  });

  // subscribe to commands from localstorage port
  app.ports.saveSession.subscribe(s => window.localStorage.setItem("cactus-session", s));
}

window.initComments = initComments;
