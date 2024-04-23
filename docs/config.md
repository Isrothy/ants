# Configuration

Configuration files for ants are located in `.ants/config.json`. Below is the
default configuration, illustrating typical settings and their syntax.

**Default Configuration Example:**

```json
{
  "template": {
    "name": "John",
    "email": "john@example.com",
    "dateFormat": "%Y-%m-%d",
    "timeFormat": "%H:%M:%S",
    "dateTimeFormat": "%Y-%m-%dT%H:%M:%S",
    "variables": {
      "game": "League of Legends"
    }
  },
  "extensions": [
    "gfm",
    "attributes",
    "auto_identifiers_ascii",
    "autolinks",
    "wikilinks_title_before_pipe"
  ]
}
```

## Configuration Options

### template

This section configures the default settings for note templates:

- **`name`**: Name of the person creating the note.
- **`email`**: Email address of the note creator.
- **`dateFormat`**: Format for displaying dates (e.g., "2024-01-01").
- **`timeFormat`**: Format for displaying time (e.g., "13:45:00").
- **`dateTimeFormat`**: Combined date and time format (e.g., "2024-01-01T13:45:00").
- **`variables`**: A dictionary of custom variables that can be used within
  templates (e.g., favorite game).

### extensions

This section lists enabled Markdown syntax extensions, enhancing the default
Markdown capabilities:

- **Standard Extensions**:
  - `autolinks`: Automatically converts URLs and emails into links.
  - `pipe_tables`: Enables GitHub Flavored Markdown (GFM) style tables.
  - `hard_line_breaks`: Treats new lines in paragraph-like hard breaks.
  - `strikethrough`: Adds strikethrough formatting using `~~text~~`.
  - `superscript`: Adds superscript formatting using `^text^`.
  - `subscript`: Adds subscript formatting using `~text~`.
  - `smart`: Applies typographic improvements like smart quotes.
  - `math`: Supports inline and display math using LaTeX style.
  - `emoji`: Converts `:emoji:` codes into actual emojis.
  - `footnotes`: Enables footnote formatting.
  - `definition_lists`: Supports definition lists.
  - `fancy_lists`: Supports advanced list formatting.
  - `task_lists`: Adds checkboxes for task lists.
- **Advanced Attributes**:
  - `attributes`: Allows adding custom attributes to elements.
  - `raw_attribute`: Permits raw HTML or LaTeX.
  - `bracketed_spans`: Enables spans wrapped in brackets.
  - `fenced_divs`: Supports divisions wrapped in code fences.
  - `auto_identifiers`: Generates automatic ID attributes for headers.
  - `auto_identifiers_ascii`: Uses ASCII-only characters for automatic IDs.
  - `implicit_heading_references`: Allows headings to be used as implicit reference links.
- **Wikilinks**:
  - `wikilinks_title_before_pipe`: Enables Wikilink support with title before pipe.
  - `wikilinks_title_after_pipe`: Enables Wikilink support with title after pipe.
- **Additional Markdown Styles**:
  - `alerts`: Enables custom alert boxes in Markdown.
  - `gfm`: Enables GitHub Flavored Markdown extensions.

By adjusting these settings in the configuration file, users can customize the
behavior of ants to suit their specific note-taking needs.

