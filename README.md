![](./assets/readme-header.png)

An embeddable web client for federated comments using the Matrix network.

[![pipeline status](https://gitlab.com/cactus-comments/cactus-client/badges/main/pipeline.svg)](https://gitlab.com/cactus-comments/cactus-client/-/commits/main)
[![](https://img.shields.io/badge/chat-%23cactus%3Acactus.chat-informational)](https://matrix.to/#/%23cactus:cactus.chat)


There are two components to Cactus Comments: 

- The embeddable web client (this repo)
- The server-side appservice ([found here](https://gitlab.com/cactus-comments/cactus-appservice))

This repository is only the web client.


# Example Usage

Here is a minimal HTML page with the latest Cactus Comments release:

```html
<script type="text/javascript" src="https://latest.cactus.chat/cactus.js"></script>
<link type="text/css" rel="stylesheet" href="https://latest.cactus.chat/style.css">
<div id="comment-section">Loading Comments...</div>
<script>
  initComments({
    node: document.getElementById("comment-section"),
    defaultHomeserverUrl: "https://matrix.cactus.chat:8448",
    serverName: "cactus.chat",
    siteName: "ExamplePage",
    commentSectionId: "ExampleSection"
  })
</script>
```

This is a valid configuration for the client. You also need to use an appservice.
You can host your own, or use the public one at cactus.chat.
Check out the [Quick Start guide](https://cactus.chat/docs/getting-started/quick-start/) on our website for a more complete tutorial.


# Documentation

The complete documentation is available at our website, [cactus.chat](https://cactus.chat)

You can play with a live demo at [cactus.chat/demo](https://cactus.chat/demo/)


## Getting the JS and CSS

To get the latest release of the web client and default stylesheet, include this HTML:

```html
<script type="text/javascript" src="https://latest.cactus.chat/cactus.js"></script>
<link type="text/css" rel="stylesheet" href="https://latest.cactus.chat/style.css">
```

The Cactus Comments web client is available on the IPFS network.
CIDs for the different versions are listed on [the releases page.](https://gitlab.com/cactus-comments/cactus-client/-/releases)

The latest release is always at https://latest.cactus.chat/ (using Cloudflare's IPFS Gateway),
or using any other IPFS gateway at /ipns/latest.cactus.chat

You can also get them from
[any other IPFS gateway](https://ipfs.github.io/public-gateway-checker/).


## Initialize comments

Once you've included the javascript file, initialize the comments section with a snippet that looks something like this:

```javascript
initComments({
  node: document.getElementById("comment-section"),  // HTML element to make comments section in
  defaultHomeserverUrl: "https://matrix.cactus.chat:8448",  // full url of the Matrix server to use as guest
  serverName: "cactus.chat",            // server name of the Matrix server w/ Cactus Appservice
  siteName: "MyBlog",                   // the name that you registered with the cactus appservice
  commentSectionId: "NovemberBlogpost"  // unique ID for this comments section.
})
```

These requirements apply:

- `node` must be a HTML element present on the page.
- `defaultHomeserverUrl` must be a full url (schema, port and all) with *no trailing slash*.
- `defaultHomeserverUrl` must point to a Matrix server with guest registration enabled (enable in Synapse's `homeserver.yaml`).
- `serverName` must be the server name (domain part) of a Matrix server running the [Cactus Appservice](https://gitlab.com/cactus-comments/cactus-appservice).
- `siteName` should be the name that you register with the [Cactus Appservice](https://gitlab.com/cactus-comments/cactus-appservice).
- `commentSectionId` should be a unique string that identifies this comment section. You probably want to change this on every page.

In a typical installation, you will set everything except
`commentSectionId` statically, and provide a different `commentSectionId` for every page.
A simple way of doing this could be to set `commentSectionId: document.URL`.
