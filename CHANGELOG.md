# Changelog

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
