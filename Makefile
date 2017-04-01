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

.PHONY: test
test: build
	. ./cfg/dev && stack test

.PHONY: test-live
test-live: build
	. ./cfg/dev && stack exec ghmm-test-exe

.PHONY: lint
lint:
	find . -type f -iname '*.hs' -print0 | xargs -0 hlint

.PHONY: stylish_haskell_install
stylish_haskell_install:
	stack install stylish-haskell

.PHONY: stylish_haskell
STYLISH=stylish-haskell -i {} \;
stylish_haskell: stylish_haskell_install
	find . -type f -iname "*.hs" -exec $(STYLISH) && git diff --exit-code


.PHONY: docker-build
docker-build: build test
	stack image container

.PHONY: docker-push
docker-push: docker-build
	docker push ulfs/ghmm:latest


$(PING_URL_FILE):
	. ./cfg/dev && \
	[ -f $(PING_URL_FILE) ] && echo "$(PING_URL_FILE) still exists?!" && exit 1 ;\
	[ ! -f $(PING_URL_FILE) ] && $(SCRIPTDIR)/create-hook.sh > $(PING_URL_FILE)


.PHONY: ping-url
ping-url: $(PING_URL_FILE)
	$(eval PING_URL := $(shell cat $(PING_URL_FILE)))


.PHONY: hook-create
hook-create:
	@. ./cfg/dev && \
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
	. ./cfg/dev && \
        beame-insta-ssl tunnel $$PORT http --fqdn $$BEAME_FQDN
