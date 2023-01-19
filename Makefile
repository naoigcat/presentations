.PHONY: serve
serve:
	docker run --rm --init -itv ${PWD}:/src/site -p 80:4000 naoigcat/github-pages

.PHONY: preview
preview:
	docker run --rm --init -itv ${PWD}:/home/marp/app -p 80:8080 marpteam/marp-cli .

.PHONY: generate
generate:
	docker run --rm --init -itv ${PWD}:/home/marp/app marpteam/marp-cli --server false --output docs .
