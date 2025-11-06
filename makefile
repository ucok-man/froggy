# Include .env if exists
-include .env

# ------------------------------------------------------------------ #
#                               HELPERS                              #
# ------------------------------------------------------------------ #

## help: print this help message
.PHONY: help
help:
	@echo 'Usage:'
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' | sed -e 's/^//'


# ------------------------------------------------------------------ #
#                             API SCRIPT                             #
# ------------------------------------------------------------------ #
## test: run all available test
.PHONY: test
test:
	@echo 'running test...'
	@gotestdox -v -count=1 ./...

.PHONY: run
run:
	@go run cmd/main.go