# builder

A pipeline runner for Claude-based build automation.
Described here https://eighttrigrams.substack.com/p/running-in-circles

## Getting started

```bash
bbin install .
```

Run a pipeline:

```bash
builder <pipeline-name> <commit-message-prefix> [port]
```

Pipelines with `:standard-fullstack? true` require a port and a Makefile with `start`, `stop`, and `test` targets.

Generate mermaid diagram of a pipeline:

```bash
bb -m builder.mermaid <pipeline-name> [output-file]
```

### Bundled pipelines

- `fullstack-exploratory-first-with-boyscout-help` - Exploratory-first workflow with code review and boyscout refactoring

## Validate a pipeline

```bash
bb validate <pipeline-name>
```

## Testing

```bash
bb test
```

# Rationale

- Never do many things at once, constantly refine.
