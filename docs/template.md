# Template

A template language is employed by the ants note generator. Templates are
stored in `.ants-templates/default.md`.

## Syntax

The syntax for templates uses placeholders enclosed in double curly braces:

```
{{placeholder}}
```

### Example

Consider a template file with the following content:

```markdown
Hello, my name is **{{NAME}}**.
```

During note generation, the `{{NAME}}` placeholder will be replaced by the
actual value provided. For example, when using the command:

```bash
ants new -t "new note" --name "John" .
```

The generated note would look like:

```markdown
Hello, my name is **John**.
```

If no name is provided in the command, the value configured in
`.ants/config.json` will be used instead.

## Default Placeholders

The following placeholders are available by default and can be used in any template:

- **NAME**: The name of the person creating the note.
- **EMAIL**: The email address of the note's creator.
- **DATE**: The current date, formatted according to the `dateFormat` in the
  configuration file.
- **TIME**: The current time, formatted according to the `timeFormat` in the
  configuration file.
- **DATETIME**: The current date and time, formatted according to the
  `dateTimeFormat` in the configuration file.

### Formatting Dates and Times

When using the `DATE`, `TIME`, or `DATETIME` placeholders, ensure they are
formatted using the date and time formats specified in `.ants/config.json`.

## Custom Placeholders

You can define your own placeholders within `.ants-templates` under the
`template.variables` section or directly in the command line:

```bash
ants new -t "new note" VARNAME=VALUE .
```

This flexibility allows for highly customizable template usage, adapting to
various note-taking needs and preferences.

