# Presentations

Presentations with [Marp](https://github.com/marp-team/marp)

## Commands

### Serve pages by jekyll

```sh
make serve
```

### Preview presentations by marp

```sh
make preview
```

### Generate presentations by marp

```sh
make generate
```

## Notes

### `NotAllowedError: Permission was denied` warning in browser console

When previewing presentations, `NotAllowedError: Permission was denied` warning may appear in the browser console.
This warning is related to the Screen Wake Lock API, which requires a secure HTTPS environment to function properly.
However, this warning does not affect the display of the presentations and can be safely ignored.
