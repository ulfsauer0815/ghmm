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
	stack exec ghmm-exe cfg/dev.yml | tee -a server.log

.PHONY: test
test: build
	stack test

.PHONY: test-live
test-live: build
	stack exec ghmm-test-exe cfg/dev.yml

.PHONY: hlint-install
hlint-install:
	stack install hlint

.PHONY: hlint
hlint:
	hlint -h .hlint .

.PHONY: lint
lint: hlint

.PHONY: hlint-apply-refact
HLINT=hlint --refactor --refactor-options -i {} \;
hlint-apply-refact:
	find . -type f -iname "*.hs" -exec $(HLINT)

.PHONY: hlint-apply-refact-install
hlint-apply-refact-install: hlint-install
	stack install apply-refact

.PHONY: stylish-haskell-install
stylish-haskell-install:
	stack install stylish-haskell

.PHONY: stylish-haskell
STYLISH=stylish-haskell -i {} \;
stylish-haskell:
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
