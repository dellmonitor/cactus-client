import { Elm } from './Main.elm'

function initComments({node, defaultHomeserverUrl, siteName, uniqueId}) {
  // make a comments section at dom node `node`
  var app = Elm.Main.init({
    node: node,
    flags: {
      defaultHomeserverUrl: defaultHomeserverUrl,
      siteName: siteName,
      uniqueId: uniqueId
    }
  });
  console.log("made elm app!")
}

window.initComments = initComments;
