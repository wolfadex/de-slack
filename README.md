# De-Slack

[![Netlify Status](https://api.netlify.com/api/v1/badges/1d10038a-fe84-464e-9e8d-b6cab7de21b4/deploy-status)](https://app.netlify.com/sites/de-slack/deploys)

A proof of concept decentrailized chat app

## Dev

- Run `yarn` to install all JavaScript dependencies
- Run `yarn dev` to start up the dev server
- Navigate to `localhost:8000/server.html` to start the chat server
- Navigate to `localhost:8000` to start a chat client


## How it works

- Copy the address from the server
- Paste the server's address into the input on the client and press `Enter`/`Return`
- This connects the client to the server
- New users must sign up (existing user login is in progress)
- The server must approve the sign up
- Connect as many clients as you want
- Fill in the message input and press `Enter`/`Return` to send the message

## Todo

- [x] Test outside of localhost
- [x] Sign-up
- [ ] Login
- [ ] Customize username
- [ ] Storing state of server
- [ ] Exporting server data
- [ ] Importing server data
- [ ] Creating channels
- [ ] Direct messages

<br />

Thank you to the [bugout](https://github.com/chr15m/bugout) project for making this possible.