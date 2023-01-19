.PHONY: serve
serve:
	docker run --rm --init -itv ${PWD}:/src/site -p 4000:4000 naoigcat/github-pages

.PHONY: preview
preview:
	docker run --rm --init -itv ${PWD}:/home/marp/app -p 8080:8080 marpteam/marp-cli .

.PHONY: generate
generate:
	docker run --rm --init -itv ${PWD}:/home/marp/app -p 8080:8080 marpteam/marp-cli --server false --output docs .

.PHONY: clean
clean:
	find docs -type d -regex 'docs/[0-9]*' | xargs rm -rf
