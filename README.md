![](./assets/readme-header.png)

An embeddable web client for federated comments using the Matrix network.

[![pipeline status](https://gitlab.com/cactus-comments/cactus-client/badges/master/pipeline.svg)](https://gitlab.com/cactus-comments/cactus-client/-/commits/master)


# Usage

## Include JS

Include the client JS file somwhere in your HTML page / template:

```html
<script type="text/javascript" src="https://gateway.pinata.cloud/ipfs/Qmbnyp39DR96qQJ2E3H2Ms2Y5DSLgaCDq4xGTASdf4MiHg"></script>
```
(This link is not continously updated, and may be out of date as you read this.)


## Include CSS

You also need to include a stylesheet. We do not host the stylesheet anywhere,
so you will need to download it from this repo `src/style.css` and add it to
your website.


## Initialize comments

Initialize the comments section with a JS snippet that looks something like this:

```javascript
initComments({
  node: document.getElementById("comment-section"),
  defaultHomeserverUrl: "https://cactus.chat:8448",  // full url of the Matrix server to use as guest
  serverName: "cactus.chat",            // server name of the Matrix server w/ Cactus Appservice
  siteName: "MyBlog",                   // name of your website. used for moderation namespacing 
  commentSectionId: "NovemberBlogpost"  // unique ID for this comments section.
})
```

Note that the following requirements apply:

- `node` must be a HTML element present on the page.

- `defaultHomeserverUrl` must be a full url (schema, port and all) with *no trailing slash*.

- The Matrix server at `defaultHomeserverUrl` must have guest registration enabled (enable in Synapse's `homeserver.yaml`).

- `serverName` must be the server name (domain part) of a Matrix server running the [Cactus Appservice](gitlab.com/cactus-comments/cactus-appservice).

- `serverName` may be a different Matrix server than `defaultHomeserverUrl` - but does not have to be.

- `siteName` should be the name that you register with the [Cactus Appservice](gitlab.com/cactus-comments/cactus-appservice).

- `commentSectionId` should be a unique string that identifies this comment section. You probably want to change this on every page.

In a typical installation, you will want to set everything except
`commentSectionId` statically, and somehow provide a different `commentSectionId` for every page.
A simple way of doing this could be to set `commentSectionId: document.URL`.
