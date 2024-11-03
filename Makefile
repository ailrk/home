CABAL ?= cabal
CABAL_BUILD_PARALLEL ?= $(shell nproc)
GHC_MAJOR_VERSION := $(shell ghc --numeric-version | cut -d'.' -f1)

check:
	cabal run -- website check

watch:
	cabal run -- website watch

build:
	cabal run r2book
	cabal run -- website build

clean:
	cabal run -- website clean
