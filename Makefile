# Makefile for blog-rekahsoft-ca

.PHONY: all configure clean build site deploy preDeploy test server

all: clean build test

configure:
	cabal configure --enable-tests

clean:
	rm -Rf _site _cache
	cabal clean

build: configure
	cabal build

site: build
	./site rebuild

deploy: site preDeploy test
	@echo "Deploying website..."
	@rsync -rpogtzcv --delete -e ssh _site/ collin@rekahsoft.ca:~/public_html/blog/

preDeploy: _site
	@echo "Removing empty files..."
	@find _site -type f -empty -exec rm -v {} \;
	@echo "\n"

test: site preDeploy
	cabal test --show-details=always --test-option=--color

server: build site
	./site server
