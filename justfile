# Build all packages
build:
    cabal build all -O0

# Run tests
test:
    cabal test all -O0 --test-show-details=direct

# Format Haskell sources
format:
    fourmolu -i lib/**/*.hs test/*.hs app/*.hs

# Lint Haskell sources
lint:
    hlint lib/

# Format cabal file
cabal-fmt:
    cabal-fmt -i kel-circle.cabal

# Build Lean proofs
lean:
    cd lean && lake build

# Build PureScript client
build-client:
    cd client && npm install && spago build

# Bundle PureScript client
bundle-client:
    cd client && spago bundle -p kel-circle-trivial

# Format PureScript sources
format-client:
    cd client && purs-tidy format-in-place "kel-circle-client/src/**/*.purs" "kel-circle-trivial/src/**/*.purs"

# Lint PureScript sources
lint-client:
    cd client && spago build

# Test PureScript client
test-client:
    cd client && spago test -p kel-circle-client

# Full CI check
ci: format cabal-fmt lint build test lean build-client test-client

# Build documentation
build-docs:
    mkdocs build --config-file docs/mkdocs.yml

# Serve documentation locally
serve-docs:
    mkdocs serve --config-file docs/mkdocs.yml

# Deploy documentation
deploy-docs:
    mkdocs gh-deploy --force --config-file docs/mkdocs.yml

# Run the server (with static file serving)
serve port="8080" db="kel-circle.db" pass="bootstrap": bundle-client
    cabal run kel-circle-server -O0 -- {{port}} {{db}} {{pass}}

# Restart the server (rebundle + relaunch)
restart port="8080" db="kel-circle.db" pass="bootstrap": bundle-client
    -pkill -f "kel-circle-server"
    cabal run kel-circle-server -O0 -- {{port}} {{db}} {{pass}}

# Clean build artifacts
clean:
    cabal clean
    cd lean && lake clean
    cd client && rm -rf .spago output node_modules
