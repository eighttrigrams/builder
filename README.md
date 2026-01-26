# builder

A pipeline runner for Claude-based build automation.

## Installation

```bash
bbin install .
```

## Usage

Run a pipeline:

```bash
builder <pipeline-name> <commit-message-prefix>
```

Generate mermaid diagram of a pipeline:

```bash
builder-mermaid <pipeline-name> [output-file]
```

## Bundled pipelines

- `tracker-build` - Build pipeline for the tracker project
