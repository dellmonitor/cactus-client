import { Elm } from './Main.elm'

function initComments({node, defaultHomeserverUrl, serverName, siteName, commentSectionId}) {
  console.log("loaded session: " + JSON.stringify(localStorage.getItem("cactus-session")));
  console.log("loaded session: " + localStorage.getItem("cactus-session"));

  // make a comments section in DOM element `node`
  // initialize with provided config, and nullable session from localstorage
  var app = Elm.Main.init({
    node: node,
    flags: {
      defaultHomeserverUrl: defaultHomeserverUrl,
      serverName: serverName,
      siteName: siteName,
      commentSectionId: commentSectionId,
      storedSession: localStorage.getItem("cactus-session") 
    }
  });

  // subscribe to commands from localstorage port
  app.ports.storeSession.subscribe(s => {
    window.localStorage.setItem("cactus-session", s);
    console.log("stored " + JSON.stringify(s));
  });
}

window.initComments = initComments;
