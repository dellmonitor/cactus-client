import { Elm } from './Main.elm'

function initComments({node, defaultHomeserverUrl, siteName, uniqueId, serverName}) {
  // make a comments section at dom node `node`
  var app = Elm.Main.init({
    node: node,
    flags: {
      defaultHomeserverUrl: defaultHomeserverUrl,
      siteName: siteName,
      uniqueId: uniqueId,
      serverName: serverName
    }
  });
  console.log("made elm app!")
}

window.initComments = initComments;
