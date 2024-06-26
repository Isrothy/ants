# ants

**ants - A Note Taking System**

ants is a lightweight, command-line note-taking system written in Haskell.

## Highlights
- **Implemented in Haskell**
- **Lightweight and efficient**
- **Text editor integration**
  - [vscode extension](https://github.com/dsgsjk/ants-vscode)
- **Language server features**:
  - Auto-completion
  - Hover information
  - Go to Definition
  - Find References
- **Creating notes from templates**
- **Advanced filters**
- **Mind mapping**
- **Extensive Markdown grammar support** - See [config.md](docs/config.md)

## Installation

Ensure you have [stack](https://docs.haskellstack.org/en/stable/) installed.

```bash
stack install
```

## Usage

### Initializing

```bash
ants init
```

Initializes an empty Note Taking System in the current directory.

### Creating Notes from Templates

```bash
ants new -t "My Note" DIR
```

This command creates a new note titled "My Note" in the directory specified by
`DIR`, using predefined templates.

#### Available options:

```
-t, --title TITLE               Title of the note
-n, --name NAME                 Name of the author
-e, --email admin@example.com   Email of the author
DIR                             Directory to create the note
VARNAME=VALUE                   Variables to override default config settings
-h, --help                      Show help information
```

### Filter

To filter and sort notes:

```bash
ants list [-s|--sort FIELD] [-f|--filter FILTER]
```

For detailed query language options, see [query.md](docs/query.md).

### Mind Map

To generate a mind map of all notes in the current directory in Graphviz's dot
language:

```bash
ants graph
```

To export the mind map as an SVG file:

```bash
ants graph --svg
```

Make sure you have [Graphviz](https://graphviz.org/) installed and in your PATH.

### Shell completions

#### Bash

Add the following to your `~/.bashrc`:

```bash
source <(ants --bash-completion-script `which ants`)
```

And restart the shell.

#### Zsh

##### Oh-my-zsh

```zsh
ants --zsh-completion-script `which ants` > ~/.oh-my-zsh/completions/_ants
```

##### Generic

```zsh
mkdir -p ~/.ants-completions
ants --zsh-completion-script `which ants` > ~/.ants-completions/_ants
```

Then add the following to the **start** of your `~/.zshrc`:

```zsh
fpath=(~/.ants-completions $fpath)
```

If this still does not work after restarting the shell, you might need to add the following to the **end** of your `~/.zshrc`:

```zsh
autoload -Uz compinit
compinit
```

And restart the shell again.

#### Fish

```fish
mkdir -p ~/.config/fish/functions
ants --fish-completion-script $(which ants) > ~/.config/fish/functions/ants.fish
```

And restart the shell.

## Configuration

Configuration files are located at `.ants/config.json`.

For more information, see [config.md](docs/config.md).

