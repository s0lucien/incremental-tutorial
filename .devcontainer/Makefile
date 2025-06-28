VERSION=5.3.20250628
docker-build-dev :
	docker build . -f=Dockerfile.dev -t ocaml-devcontainer:latest

docker-tag-dev :
	docker tag ocaml-devcontainer:latest s0lucien/ocaml-devcontainer:$(VERSION)