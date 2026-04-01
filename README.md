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

### `ResizeObserver loop completed with undelivered notifications.` error in browser console

When previewing presentations, `ResizeObserver loop completed with undelivered notifications.` error may appear in the browser console.
This error is caused by Marp's auto-scaling behavior in the generated slide runtime and may be reported by browsers during layout updates.
However, this error does not affect the display of the presentations and can be safely ignored.
