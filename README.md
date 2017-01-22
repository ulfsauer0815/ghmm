# ghmm

A GitHub event bot for Mattermost.

## Description

Relays GitHub webhook events to your Mattermost incoming webhook.

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

The [mattermost documentation](https://docs.mattermost.com/developer/webhooks-incoming.html)
explains how to do that to some degree.

You will get an address which looks similar to this:
http://mattermost.hostname.com/hooks/xxx-generatedkey-xxx


## Deploy `ghmm`

### Install [`stack`](https://www.haskellstack.org/)

Stack automatically downloads GHC and builds the project.

Follow the [instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install)
to install it.

### Build the Project

`stack build`

The output will also tell you where to find the executable.

### Create an environment

You need various settings to properly start ghmm, most of them are
self-explanatory.

| Variable             | Description
| -------------------- | ----------------------------------------------------- |
| `PORT`               | Port to start ghmm on                                 |
| `GITHUB_SECRET`      | The secret you chose to share between GitHub and ghmm |
| `MATTERMOST_URL`     | The full url where ghmm is available                  |
| `MATTERMOST_PORT`    | Port where mattermost is available, usually 443       |
| `MATTERMOST_API_KEY` | The value at the end of the incoming webhook url      |

See `config/dev.example` for how this might look like.

Note that this example config also includes values for the development scripts,
which might not be relevant for you.

### Start the app

`stack exec ghmm-exe`

Make sure you have configured your environment.
If it is all set in a file like `config/dev.example`, then you can just load it like this:

`. config/dev.example && stack exec ghmm-exe`.

Now everythign should be set up.

## Local Deployment
If you want to test it locally, you can use tunneling services such as
beameio.net, ngrok.io, pagekite.me, localtunnel.me.

*Note*:
I personally use beameio.net, because it works great within different networks
and they provide a static address for free. Otherwise you have to delete and add
the new address to your GitHub repo(s) every time you disconnect.
