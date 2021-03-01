# Changelog

## 0.4.2

- Can now display multiple error messages at the same time.
- Better error messages for common bad config values.
- `m.notice` messages render now.
- Config parsing moved out of javascript, into Elm.
- Bugfix: Correct a hardcoded string oversight, that caused incorrect displaynames on Emote messages.

## 0.4.1

- Bugfix: Move the right sync token when getting newer messages, preventing duplicate comments after posting in small rooms.

## 0.4.0

- Fetch new messages after successfully posting a comment
- Update current time periodically
- Error messages are now red, and can be closed
- Bugfix: don't crash on redactions
- Bugfix: guest users can view messages sent by others after posting anonymously

## 0.3.2

- CI changes: put IPFS gateway links in release description

## 0.3.1

- Don't fetch messages again after an empty chunk was received
- AGPL -> GPL3

## 0.3.0

- Always join users. Issue join API call on login and on user session deserialization

## 0.2.1

- Fix Gitlab CI release pipeline for pinning artifacts to IPFS.

## 0.2.0

- Get a consistent number of comments, irrespective of how many unrenderable
  events are in the room.
- Introduce optional `pageSize` configuration parameter, which sets pagination size.
