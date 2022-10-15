# core

.PHONY: build clean test repl watch cic ci formatc format lint lintc haddock haddockc hackage

ARGS = ""

build:
	if [ -z "$(ARGS)" ]; then \
		cabal build; \
	else \
		cabal build $(ARGS); \
	fi

clean:
	cabal clean

test:
	if [ -z "$(ARGS)" ]; then \
		cabal test; \
	else \
		cabal test $(ARGS); \
	fi

repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl; \
	else \
		cabal repl $(ARGS); \
	fi

watch:
	ghcid --command "cabal repl $(ARGS)"

# ci

cic: formatc lintc haddockc

ci: lint format

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.6.1#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#ormolu -- --mode check ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#nixpkgs-fmt -- --check

format:
	nix run github:tbidne/nix-hs-tools/0.6.1#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#ormolu -- --mode inplace ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#nixpkgs-fmt

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.6.1#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.6.1#hlint

# generate docs for main package, copy to docs/
haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.4/classy-containers-0.1/opt/doc/html/classy-containers/* docs/

haddockc:
	nix run github:tbidne/nix-hs-tools/0.6.1#haddock-cov -- .

# generate dist and docs suitable for hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
