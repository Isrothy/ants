# Frontmatter

Frontmatter is a YAML file used to store metadata about a note.

**Example of Frontmatter:**

```yaml
title: "Hello, World!"
author: "John Doe"
date: 2022-01-01
tags:
  - hello
  - world
description: "This is my first note."
```

### Fields

The Frontmatter may contain the following fields:

- **`title`**: The title of the note.
- **`author`**: The author of the note.
- **`date`**: The creation date of the note.
- **`tags`**: An array of tags associated with the note.
- **`description`**: A brief description of the note.

### Embedding Frontmatter

Frontmatter is embedded at the beginning of the note's content as shown below:

```markdown
---
title: "Hello, World!"
author: "John Doe"
date: 2022-01-01
tags:
  - hello
  - world
description: "This is my first note."
---

# Hello, World!

Some content.
```

This section demonstrates how to incorporate essential metadata directly into
your note using YAML formatted Frontmatter.

