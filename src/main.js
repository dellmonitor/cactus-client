
import { Elm } from './Main.elm'
import matrix from 'matrix-js-sdk';

function initComments({node, defaultHomeserverUrl}) {
  // make a comments section at dom node `node`
  // TODO: argument validation

  var app = Elm.Main.init({
    node: node,
    flags: { defaultHomeserverUrl: defaultHomeserverUrl }
  });
  console.log("made elm app!")

  const client = matrix.createClient(defaultHomeserverUrl);
  console.log("made matrix client")
}

window.initComments = initComments;
