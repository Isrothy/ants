# Query

The query language is inspired by
[Obsidian](https://help.obsidian.md/Plugins/Search) and supports several types
of query terms and operators to provide users with more precise control over
their searches.

## Query Terms

The query language includes four kinds of terms:

- **Case Insensitive Term**
  - **Syntax**: A string surrounded by `"`, or a string consisting of only digits and English letters.
  - **Examples**: `apple`, `"Apple"`
  - **Matching**: `'apple'` matches `"Apple"`, `apple`, and `APPLE`.
- **Case Sensitive Term**
  - **Syntax**: A string surrounded by `'`.
  - **Examples**: `'apple'`
  - **Matching**: `"Apple"` matches exactly `"Apple"` but not `apple` or `APPLE`.
- **Fuzzy Term**
  - **Syntax**: A string surrounded by `~`.
  - **Examples**: `~apple~`
  - **Matching**: Editing distance is applied in fuzzy matching.
- **Regular Expression Term**
  - **Syntax**: A string surrounded by `/`.
  - **Examples**: `/[a-z]+/`

## Query Operators

Query operators refine searches by applying filters based on specific document characteristics:

- **`content`**: Filters documents containing specified terms.
  - **Example**: `content:algorithm`
- **`tasks`**: Filters documents containing tasks with specified terms.
  - **Examples**: `tasks:"meeting preparation"`, `task-todo:"write reports"`, `task-done:"read books"`
- **`alert`**: Filters documents containing alerts with specified terms.
  - **Example**: `alert-important:"safety instructions"`
- **`has-link`**: Filters documents containing links to other documents or bookmarks.
  - **Examples**: `hasLink:some/directory/some/document.md`, `haslink:some/directory/some/document.md#tag`
- **`author`**: Filters documents by author.
  - **Example**: `author:Jone`
- **`title`**: Filters documents by title.
  - **Example**: `title:"Annual Report"`
- **`tag`**: Filters documents by tags.
  - **Example**: `tag:"finance"`
- **`description`**: Filters documents by descriptions.
  - **Example**: `description:~project overview~`
- **`date`**: Filters documents by creation date or date range.
  - **Examples**: `date:[2024-01-01,2024-12-31]`, `date:[2024-02-29]`, `date:[,2024-12-31]`, `date:[2024-01-01]`
- **`in-directory`**: Filters documents located within a specific directory.
  - **Example**: `in-directory:some/directory`

## Boolean Operations

Boolean operations follow syntax similar to the C programming language:
- `&&` for `AND`
- `||` for `OR`
- `!` for `NOT`

## Example Usage
```
!author:/^Joshua$/ && (content:~guide~ || (date:[2021-01-01,2021-12-31] && tag:"Haskell"))
```
This query filters out documents whose author is not `Joshua`, containing
content similar to "guide" or documents created in 2021 with the tag "Haskell".

