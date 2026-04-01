SHELL = /bin/zsh

.PHONY: serve
serve: generate
	for port in $$(jot -r 100 $$(sysctl net.inet.ip.portrange.first | awk '{print $$2}') $$(sysctl net.inet.ip.portrange.last | awk '{print $$2}')) ; \
	do \
		netstat -a -n | grep "\*\.$$port.*LISTEN" > /dev/null || break ; \
	done ; \
	exec {fd}< <( \
		until docker logs "$$(docker ps -qf 'ancestor=naoigcat/github-pages')" 2>/dev/null | grep 'Server running' ; do sleep 1 ; done ; \
		open http://localhost:$$port ; \
	) ; \
	docker run --rm --init -itv $$(pwd):/src/site -p $$port:4000 naoigcat/github-pages || :

.PHONY: preview
preview:
	docker run --rm --init -itv ${PWD}:/home/marp/app -p 80:8080 marpteam/marp-cli . || :

.PHONY: generate
generate:
	rm -fr _posts && \
	docker run --rm --init -itv ${PWD}:/home/marp/app marpteam/marp-cli --server false --output _posts . || :
