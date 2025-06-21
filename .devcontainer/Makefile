VERSION=0.3
docker-build-dev :
	docker build . -f=Dockerfile.dev -t s0ln/ocaml-devcontainer:latest

docker-tag-dev :
	docker tag s0ln/ocaml-devcontainer:latest s0ln/ocaml-devcontainer:$(VERSION)