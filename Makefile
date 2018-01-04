all:
	docker build -f images/build-image -t postgres-k8s-export:alpine-build .
	docker run -v $(PWD):/opt/server postgres-k8s-export:alpine-build cabal install
	docker build -f images/dist-image -t postgres-k8s-export:dist .
