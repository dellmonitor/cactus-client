import { Elm } from './Main.elm'

function initComments(config) {
  // get nullable session object from localstorage
  config['storedSession'] = JSON.parse(localStorage.getItem("cactus-session"));

  // get node from the config
  // remove it from config object before passing to elm
  var node = config['node']
  delete config['node']

  // make a comments section in DOM element `node`
  // initialize with provided config
  var app = Elm.Main.init({
    node: node,
    flags: config
  });

  // subscribe to commands from localstorage port
  app.ports.storeSession.subscribe(s => localStorage.setItem("cactus-session", s));
}

// Allow specifying config using data-* attributes on the script tag
if ("node" in document.currentScript?.dataset) {
  var config = Object.assign({}, document.currentScript.dataset);
  config["node"] = document.querySelector(config["node"]);
  initComments(config);
}

window.initComments = initComments;
