![](./assets/readme-header.png)

An embeddable web client for federated comments using the Matrix network.

[![pipeline status](https://gitlab.com/cactus-comments/cactus-client/badges/master/pipeline.svg)](https://gitlab.com/cactus-comments/cactus-client/-/commits/master)
[![](https://img.shields.io/badge/chat-%23cactus%3Aolli.ng-informational)](https://matrix.to/#/%23cactus:olli.ng)


# Quick Start

Here is a minimal HTML page with Cactus Comments:

```html
<script type="text/javascript" src="https://gateway.pinata.cloud/ipfs/QmTPXPLwHvHh1SpSsqFh8BnAXMauGAEoNHEeafiB3uHSxq/0.1.0/cactus.js"></script>
<link rel="stylesheet" href="https://gateway.pinata.cloud/ipfs/QmTPXPLwHvHh1SpSsqFh8BnAXMauGAEoNHEeafiB3uHSxq/0.1.0/style.css"></script>
<div id="comment-section">Loading Comments...</div>
<script>
  initComments({
    node: document.getElementById("comment-section"),
    defaultHomeserverUrl: "https://cactus.chat:8448",
    serverName: "cactus.chat",
    siteName: "ExamplePage",
    commentSectionId: "ExampleSection"
  })
</script>
```


# Usage

There are two components to Cactus Comments: 

- The embeddable web client (this repo)
- The server-side appservice ([found here](https://gitlab.com/cactus-comments/cactus-appservice))

This guide will only show you how to set up the web client.

Go to [the appservice repository](https://gitlab.com/cactus-comments/cactus-appservice) for instructions
on setting up the appservice.


## Get JS and CSS from IPFS

To get the v0.1.0 web client and default stylesheet from
[pinata.cloud](https://pinata.cloud)'s IPFS gateway, include this HTML:

```html
<script type="text/javascript" src="https://gateway.pinata.cloud/ipfs/QmTPXPLwHvHh1SpSsqFh8BnAXMauGAEoNHEeafiB3uHSxq/0.1.0/cactus.js"></script>
<link rel="stylesheet" href="https://gateway.pinata.cloud/ipfs/QmTPXPLwHvHh1SpSsqFh8BnAXMauGAEoNHEeafiB3uHSxq/0.1.0/style.css"></script>
```

These files are pinned to IPFS, so you can also get them from
[a number of others IPFS gateways](https://ipfs.github.io/public-gateway-checker/), if you wish.


## Initialize comments

Initialize the comments section with a JS snippet that looks something like this:

```javascript
initComments({
  node: document.getElementById("comment-section"),  // HTML element to make comments section in
  defaultHomeserverUrl: "https://cactus.chat:8448",  // full url of the Matrix server to use as guest
  serverName: "cactus.chat",            // server name of the Matrix server w/ Cactus Appservice
  siteName: "MyBlog",                   // name of your website. used for moderation namespacing 
  commentSectionId: "NovemberBlogpost"  // unique ID for this comments section.
})
```

Note that the following requirements apply:

- `node` must be a HTML element present on the page.
- `defaultHomeserverUrl` must be a full url (schema, port and all) with *no trailing slash*.
- `defaultHomeserverUrl` must point to a Matrix server with guest registration enabled (enable in Synapse's `homeserver.yaml`).
- `serverName` must be the server name (domain part) of a Matrix server running the [Cactus Appservice](https://gitlab.com/cactus-comments/cactus-appservice).
- `serverName` may be a different Matrix server than `defaultHomeserverUrl` - but does not have to be.
- `siteName` should be the name that you register with the [Cactus Appservice](https://gitlab.com/cactus-comments/cactus-appservice).
- `commentSectionId` should be a unique string that identifies this comment section. You probably want to change this on every page.

In a typical installation, you will want to set everything except
`commentSectionId` statically, and somehow provide a different `commentSectionId` for every page.
A simple way of doing this could be to set `commentSectionId: document.URL`.
