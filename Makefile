#!/usr/bin/env make

WORKDIR=scripts
SCRIPTDIR=scripts
CONFIGDIR=scripts
PING_URL_FILE = $(WORKDIR)/HOOK
REPOS_FILE=$(CONFIGDIR)/REPOS


.PHONY: build
build:
	stack build

.PHONY: run
run:
	. ./cfg/dev && stack exec ghmm-exe | tee -a server.log


$(PING_URL_FILE):
	. ./cfg/dev && \
	[ -f $(PING_URL_FILE) ] && echo "$(PING_URL_FILE) still exists?!" && exit 1 ;\
	[ ! -f $(PING_URL_FILE) ] && $(SCRIPTDIR)/create-hook.sh > $(PING_URL_FILE)


.PHONY: ping-url
ping-url: $(PING_URL_FILE)
	$(eval PING_URL := $(shell cat $(PING_URL_FILE)))


.PHONY: hook-create
hook-create:
	. ./cfg/dev && \
	[ -f $(PING_URL_FILE) ] && echo "$(PING_URL_FILE) still exists?!" && exit 1 ;\
	[ ! -f $(PING_URL_FILE) ] && $(SCRIPTDIR)/create-hook.sh $(REPOS_FILE) $(WEBHOOK_URL) > $(PING_URL_FILE) # XXX: $2 unused atm

.PHONY: hook-clean
hook-clean:
	. ./cfg/dev && \
	$(SCRIPTDIR)/cleanup-hooks.sh $(REPOS_FILE)
	rm $(PING_URL_FILE) || true


.PHONY: hook-ping
hook-ping: ping-url
	. ./cfg/dev && \
	$(SCRIPTDIR)/ping-hook.sh $(PING_URL)


.PHONY: tunnel
tunnel:
	beame-insta-ssl tunnel $(PORT) http --fqdn $(WEBHOOK_HOST)
