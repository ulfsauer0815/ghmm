# ghmm

[![Build Status](https://travis-ci.org/UlfS/ghmm.svg?branch=master)](https://travis-ci.org/UlfS/ghmm)

A GitHub event bot for Mattermost.

## Description

Relays GitHub webhook events to your Mattermost incoming webhook.

## Supported Events

Only a subset of the [GitHub events](https://developer.github.com/v3/activity/events/types/)
are supported right now.

- [PushEvent](https://developer.github.com/v3/activity/events/types/#pushevent)
- [PullRequestEvent](https://developer.github.com/v3/activity/events/types/#pullrequestevent)
- [PullRequestReviewEvent](https://developer.github.com/v3/activity/events/types/#pullrequestreviewevent)
- [PullRequestReviewCommentEvent](https://developer.github.com/v3/activity/events/types/#pullrequestreviewcommentevent)
- [IssueCommentEvent](https://developer.github.com/v3/activity/events/types/#issuecommentevent)
- [StatusEvent](https://developer.github.com/v3/activity/events/types/#statusevent)

Feel free to open a pull request or an issue if you need support for an event.

# Setup

## Create a GitHub Webhook

There is a [page on Github](https://developer.github.com/webhooks/creating/) on
how to do that.

The `config.url` must point to where ghmm will be available.
Make sure it is publicly accessible. If you want to test or use it locally,
see the ["Local Deployment" section](#local-deployment) below.

The `config.content_type` must be `json`.

Choose a `config.secret` between GitHub and ghmm, so that strangers can't just
imitate GitHub.

## Create a Mattermost Incoming Webhook

The [Mattermost documentation](https://docs.mattermost.com/developer/webhooks-incoming.html)
explains how to do that to some degree.

You will get an address which looks similar to this:
http://mattermost.hostname.com/hooks/xxx-generatedkey-xxx


## Deploy ghmm

### Install [`stack`](https://www.haskellstack.org/)

Stack automatically downloads GHC and builds the project.

Follow the [instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install)
to install it.

### Build the Project

`stack build`

The output will also tell you where to find the executable.

### Create an Environment

You need various settings to properly start ghmm, most of them are
self-explanatory.

| Variable             | Description
| -------------------- | ----------------------------------------------------- |
| `PORT`               | Port to start ghmm on (default: `8000`)               |
| `LOG_LEVEL`          | Log level (default: `ERROR`)\*                        |
| `GITHUB_SECRET`      | Secret you chose to share between GitHub and ghmm     |
| `MATTERMOST_URL`     | URL where Mattermost is available                     |
| `MATTERMOST_PORT`    | Port on which Mattermost is available, usually `443`  |
| `MATTERMOST_API_KEY` | Value at the end of the incoming webhook url          |

See `config/dev.example` for how this might look like.

Note that this example config also includes values for the development scripts,
which might not be relevant for you.

\* Accepted values:
`DEBUG`, `INFO`, `NOTICE`, `WARNING`, `ERROR`, `CRITICAL`, `ALERT`, `EMERGENCY`

### Start the app

You can start the app directly or create a docker container for it and run that.

#### Option 1: Run natively

```sh
. config/dev            # loads the configuration / environment variables
stack exec ghmm-exe     # starts the app on the configured port
```

#### Option 2: Run as Docker container

If you want to run the app as a docker container, you can build an image using
`stack` and run the app with the included docker-compose configuration.

```sh
stack image container   # creates a docker image 'ulfs/ghmm'
. config/dev            # loads the configuration / environment variables
docker-compose up -d    # starts the app on port 8000
```

### Local Deployment

If you want to test it locally, you can use tunneling services such as
[beame-insta-ssl](https://github.com/beameio/beame-insta-ssl), [ngrok](https://ngrok.com/),
[pagekite](https://pagekite.net/), [localtunnel](https://localtunnel.me).

*Note*:
I personally use [beame-insta-ssl](https://github.com/beameio/beame-insta-ssl),
because it works great within different networks and they provide a static
address for free. Otherwise you have to delete and add the new address to your
GitHub repo(s) every time you disconnect.
