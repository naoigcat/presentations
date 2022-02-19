.PHONY: serve
serve:
	docker run --rm --init -itv ${PWD}/docs:/srv/jekyll -p 8080:4000 jekyll/jekyll:pages jekyll serve

.PHONY: preview
preview:
	docker run --rm --init -itv ${PWD}:/home/marp/app -p 8080:8080 marpteam/marp-cli .

.PHONY: generate
generate:
	docker run --rm --init -itv ${PWD}:/home/marp/app -p 8080:8080 marpteam/marp-cli --server false --output docs .

.PHONY: clean
clean:
	find docs -type d -regex 'docs/[0-9]*' | xargs rm -rf
