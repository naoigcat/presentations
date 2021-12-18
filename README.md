# Presentations

Presentations with [Marp](https://github.com/marp-team/marp)

## Preview

```sh
docker run --rm --init -v $PWD:/home/marp/app -p 8080:8080 marpteam/marp-cli .
```

## Pages

```sh
docker run --rm --init -v $PWD/docs:/srv/jekyll -p 8080:4000 jekyll/jekyll:pages jekyll serve
```
