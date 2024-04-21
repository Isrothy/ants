# ants

## Filter

### Terms

The query language supports different kinds of terms.

- Case Sensitive Term.

  **Syntax**: A string surrounded by `"`, or a string consisting of only digits and
              English letters.
  **Examples**: `apple` or `"Apple"`
  **Matching**: `"Apple"` matches `"Apple"`, but not `apple` or `APPLE`

- Case Insensitive Term

  **Syntax**: A string surround by `'`
  **Examples**: `'apple'`
  **Matching**: `"Apple"` matches `"Apple"`, `apple` and `APPLE`

- Fuzzy Term

  **Syntax**: A string surround by `~`
  **Examples**: `~apple~`
  **Matching**: Editing distance is applied in fuzzy matching.

- Regular Expression Term

  **Syntax**: A string surround by `/`
  **Examples**: `/[a-z]+/`,

### Query

- Content

  To filter out all documents containing `apple`, use `content:apple`

- Tasks

  To filter out documents contains


### Boolean operations

The boolean operators is designed after C language, where `&&` represents
`and`, `||` represents `or` and `!` represents not

