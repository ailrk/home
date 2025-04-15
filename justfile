default:
  @just --list

check:
	cabal run -- website check

watch:
	cabal run -- website watch

build:
	cabal run r2book
	cabal run -- website build

clean:
	cabal run -- website clean
