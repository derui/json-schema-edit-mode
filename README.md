# json-schema-edit-mode

An Emacs minor mode for **writing JSON Schema itself**.

LSP servers help you write JSON *with* a schema, and structural editors help you edit JSON in general. Writing the schema is still mostly manual: keeping track of deep `properties → items → properties` nesting, keeping `required` in sync with `properties`, moving between `$ref` and `$defs`, and checking that the schema means what you intended.

`json-schema-edit-mode` lets you work with the *shape of the data* the
schema describes and the *intent* of an edit, rather than with JSON
syntax. It runs on top of `json-ts-mode`.

```
User                     object
├─ id *                  string  (uuid)
├─ nickname              string?
├─ amount                number | string      anyOf
├─ age                   integer  (0..150]
├─ tags                  string[]  unique
├─ contact *             oneOf by kind
│  ├─ ◇ email            object
│  │  ├─ kind *          = "email"
│  │  └─ address         string  (email)
│  └─ ◇ phone            object
│     ├─ kind            = "phone"
│     └─ number          string  ^\+?\d+$
└─ ⊂ extends Timestamps                       allOf
   ├─ createdAt *        string  (date-time)  ↖ Timestamps
   └─ updatedAt          string  (date-time)  ↖ Timestamps
```

## Features

- **Navigation**: `M-.` on a `$ref` jumps to its definition, and `M-?`
  lists every `$ref` to a definition, in the current file and in the
  schema files next to it. Fragments that are pointers or `$anchor`s,
  `$id`-based references and relative file references are supported.
  eldoc shows a summary of the `$ref` target.
- **Breadcrumb**: the header line shows where in the data the schema at
  point applies, for example `User › contact › ‹oneOf: email› › address
  : string`. A required property ends with `*`. It can be switched
  to the schema's JSON Pointer, or off.
- **Outline**: a side buffer shows the data shape (see above). It folds
  with outline-minor-mode, and you can edit from it. Common uses of
  oneOf / anyOf / allOf are folded into short forms: nullable, unions,
  const enums, discriminated unions (including OpenAPI `discriminator`)
  and `extends`. The same tree feeds imenu.
- **Safe edits**:
  - Adding a property updates `properties` and `required` together.
  - Renaming a property also updates `required`, `dependentRequired`,
    `dependencies` and `dependentSchemas`; removing a property cleans
    them up too.
  - Renaming a definition rewrites every `$ref` to it, optionally in
    other files too, whose names are shown before and after.
- **Constraints by type**: a menu lists the keywords that apply to the
  type at point, with their current values. Changes are written at once.
  - Ranges are edited bound by bound in a sub menu showing the current
    range, such as `(0..100]`.
  - enum is edited one value per line.
  - pattern is edited without the JSON escapes.
- **Transformations**:
  - toggle nullable, make an array of a schema, wrap a schema in
    oneOf / anyOf / allOf
  - extract a schema to `$defs` (or `definitions`), inline a `$ref`
  - add, remove, duplicate and reorder branches
  - switch between oneOf / anyOf / allOf
  - set a discriminating property, factor out common properties
- **Schema-specific lint** (flymake). It catches mistakes that
  metaschema validation does not report:
  - names in `required` missing from `properties`
  - unresolved `$ref`s and unused definitions
  - keywords that do not apply to the declared `type`
  - duplicated or missing discriminating values, and an OpenAPI
    `mapping` that disagrees with the branches
  - oneOf branches that overlap
  - allOf types that no value can satisfy, and `additionalProperties:
    false` rejecting properties defined in another branch

Edits follow the draft declared in `$schema`. For example, extraction
goes to `definitions` up to draft-07 and to `$defs` after, and a
`$ref` is made nullable with anyOf because draft-07 ignores keywords
next to `$ref`. draft-07 and 2020-12 are the primary targets.

Completion, hover and metaschema validation are deliberately left to an
LSP server such as
[vscode-json-languageserver](https://npmjs.com/package/vscode-json-languageserver)
through eglot or lsp-mode. Use [json-par](https://github.com/taku0/json-par)
for general structural editing of JSON. Both work alongside this mode.

## Requirements

- Emacs 29.1 or later, with tree-sitter support
- The `json` tree-sitter grammar (`M-x treesit-install-language-grammar RET json`)
- `transient`, which is built into Emacs 29 and later

YAML schemas are not supported.

## Installation

With `package-vc` (Emacs 29+):

```elisp
(package-vc-install "https://github.com/derui/json-schema-edit-mode")
```

With `use-package` (Emacs 30+):

```elisp
(use-package json-schema-edit-mode
  :vc (:url "https://github.com/derui/json-schema-edit-mode")
  :hook (json-ts-mode . json-schema-edit-mode-maybe))
```

Or put the repository on your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/json-schema-edit-mode")
(require 'json-schema-edit-mode)
```

## Setup

Enable the mode automatically for buffers that look like a schema. A
buffer looks like a schema when its root object has `$schema`, `$defs`
or `definitions`.

```elisp
(add-hook 'json-ts-mode-hook #'json-schema-edit-mode-maybe)
```

Or turn it on by hand with `M-x json-schema-edit-mode`.

Lint diagnostics are reported through flymake. Turn on `flymake-mode`
to see them:

```elisp
(add-hook 'json-schema-edit-mode-hook #'flymake-mode)
```

## Usage

### In the schema buffer

| Key                 | Command                     |
|---------------------|-----------------------------|
| `M-.`               | Go to the `$ref` definition |
| `M-?`               | List references to the definition or `$ref` at point |
| `M-p` / `M-n`       | Move the property at point up / down |
| `C-c ;`             | Menu of every operation (below) |

`C-c ;` is a binding a minor mode may claim by convention.  With `context-menu-mode` on, right-click
offers the common operations too.

The `C-c ;` menu:

| Key | Operation                                          |
|-----|----------------------------------------------------|
| `l` | Open the outline                                   |
| `b` | Toggle breadcrumb style (data path / JSON Pointer) |
| `w` | Copy the JSON Pointer of the schema at point       |
| `a` | Add a property                                     |
| `r` | Rename the property at point                       |
| `k` | Remove the property at point, after confirmation   |
| `M-p` / `M-n` | Move the property up / down             |
| `*` | Toggle required                                    |
| `t` | Change type                                        |
| `c` | Edit constraints…                                 |
| `N` | Toggle nullable                                    |
| `[` | Make an array of it                                |
| `W` | Wrap in oneOf / anyOf / allOf                      |
| `o` | oneOf / anyOf / allOf branches…                   |
| `x` | Extract to definitions                             |
| `i` | Inline `$ref`                                      |
| `d` | Rename definition                                  |

The branch menu (`C-c ; o`) acts on the oneOf / anyOf / allOf at
point:

| Key       | Operation                                                                               |
|-----------|-----------------------------------------------------------------------------------------|
| `a`       | Add a branch. For a discriminated union, asks the value and adds `const` and `required` |
| `k`       | Remove the branch at point                                                              |
| `y`       | Duplicate the branch at point                                                           |
| `p` / `n` | Move the branch up / down                                                               |
| `s`       | Switch between oneOf / anyOf / allOf, showing what changes                              |
| `d`       | Set a discriminating property on every branch                                           |
| `f`       | Factor out properties common to every branch                                            |
| `N`       | Toggle nullable                                                                         |

### Outline

The outline keys mirror the `C-c ;` menu (`r` renames, `*` toggles
required, `N` toggles nullable, in both):

| Key             | Operation                                                             |
|-----------------|-----------------------------------------------------------------------|
| `RET` / `C-o`   | Jump to / show the schema in the source                               |
| `TAB` / `S-TAB` | Fold                                                                  |
| `*`             | Toggle required                                                       |
| `t`             | Change type                                                           |
| `r`             | Rename the property or definition                                     |
| `a`             | Add a property next to the one at point (or into the object at point) |
| `k`             | Remove the property, keeping `required` and friends in sync           |
| `M-p` / `M-n`   | Move the property up / down                                           |
| `c`             | Edit constraints                                                      |
| `N`             | Toggle nullable                                                       |
| `[`             | Make an array of it                                                   |
| `W`             | Wrap in oneOf / anyOf / allOf                                         |
| `x`             | Extract to definitions                                                |
| `i`             | Inline `$ref`                                                         |
| `o`             | Branch menu for the oneOf / anyOf / allOf at point                    |
| `m`             | Switch allOf between "as written" and "merged"                        |
| `g`             | Refresh                                                               |
| `?`             | Describe the mode (help)                                              |

In the outline, `*` marks a required property and `→` marks a `$ref`.
`↖ Base` marks a property inherited through allOf. Editing an inherited
property edits it in its base definition. The outline redraws when the
schema changes, and folded headings stay folded.

### Constraint menu

The menu shows only the keywords that apply to the type at point:

| Type             | `l` range                          | Other keys                                                                             |
|------------------|------------------------------------|----------------------------------------------------------------------------------------|
| string           | minLength / maxLength              | `p` pattern, `f` format                                                                |
| number / integer | minimum / maximum (with exclusive) | `m` multipleOf                                                                         |
| array            | minItems / maxItems                | `u` uniqueItems, `i` items type                                                        |
| object           | minProperties / maxProperties      | `a` additionalProperties, `n` propertyNames                                            |
| every type       | —                                 | `e` enum, `+` add an enum value, `c` const, `d` default, `t` title, `D` description, `x` examples |

`l` opens a sub menu for the range.  Its header shows the current
range in the notation of the outline (`1..100`, `..100`, `(0..100]`),
pending changes included:

| Key       | Action                                          |
|-----------|-------------------------------------------------|
| `m` / `x` | Set the lower / upper bound (empty removes it)  |
| `M` / `X` | Remove the lower / upper bound                  |
| `(` / `)` | Toggle an exclusive lower / upper bound (numbers only) |
| `k`       | Remove both bounds                              |
| `RET`     | Back to the constraint menu                     |

An exclusive bound is written as `exclusiveMinimum` / `exclusiveMaximum`,
or in draft-04 as a boolean beside `minimum` / `maximum`.

Changes are kept pending until `RET` writes them as one undoable edit;
the menu header counts them.  `v` lists the pending changes before they
are written, and `Q` discards them and quits.  `C-g` also leaves the
buffer untouched; pending changes are kept until the menu is opened for
another schema, which says when it discards them.  Contradicting input,
such as a lower bound above the upper one, a negative length, or a
decimal bound on an integer, is rejected before anything is written.
`!` lists keywords that do not apply to the type, and `X` removes them.

`e` opens a buffer with one enum value per line. For strings, each line
is the string itself, without quotes. `C-c C-c` returns to the menu
with the new values and `C-c C-k` discards them. Duplicates and values
of the wrong type are flagged with flymake.

## Customization

`M-x customize-group RET json-schema-edit`. The main options are:

| Variable | Default | Meaning |
|----------|---------|---------|
| `json-schema-edit-breadcrumb-style` | `instance` | `instance` (data path), `schema` (JSON Pointer) or `nil` |
| `json-schema-edit-breadcrumb-separator` | `" › "` | Separator of the breadcrumb |
| `json-schema-edit-schema-versions` | official meta-schema URIs | Alist from a full `$schema` URI to its version (`2020-12`, `2019-09`, `draft-07`, `draft-06`, `draft-04`) |
| `json-schema-edit-reference-files-function` | JSON files in the same directory | Files searched by `M-?`, renames and the unused-definition check |
| `json-schema-edit-lint-unused-definitions` | `directory` | Count references from other files (`directory`), this file only (`file`), or disable (`nil`) |
| `json-schema-edit-indent-offset` | `nil` | Indentation of inserted text; `nil` uses `json-ts-mode-indent-offset` |
| `json-schema-edit-property-types` | JSON types | Types offered when adding a property |
| `json-schema-edit-constraint-formats` | standard formats | Values offered for `format` |
| `json-schema-edit-outline-display-action` | right side window | How the outline buffer is shown |
| `json-schema-edit-outline-select-window` | `t` | Whether `json-schema-edit-outline` moves point to the outline window |
| `json-schema-edit-outline-refresh-delay` | `0.3` | Idle seconds before the outline redraws |
| `json-schema-edit-outline-max-label-width` / `-max-type-width` | `40` / `36` | Column widths of the outline |

## Development

Tasks are run with [Task](https://taskfile.dev).

```sh
task              # run test and checkdoc in parallel
task test         # ERT tests (test/*-test.el)
task checkdoc     # fails on any checkdoc warning
task compile      # byte-compile with warnings as errors
```

### Layout

| File                             | Role                                                                                                                                          |
|----------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| `json-schema-edit-model.el`      | Schema model built from the tree-sitter tree: JSON Pointer, `$ref` index and resolution, drafts. Every other part reads the schema through it |
| `json-schema-edit-header.el`     | Breadcrumb in the header line                                                                                                                 |
| `json-schema-edit-xref.el`       | xref backend and eldoc                                                                                                                        |
| `json-schema-edit-lint.el`       | flymake backend                                                                                                                               |
| `json-schema-edit-edit.el`       | Text primitives and synced property / definition edits                                                                                        |
| `json-schema-edit-outline.el`    | Outline buffer and imenu                                                                                                                      |
| `json-schema-edit-transform.el`  | Transformations and branch editing                                                                                                            |
| `json-schema-edit-constraint.el` | Constraint menu and enum buffer                                                                                                               |
| `json-schema-edit-menu.el`       | Transient menus                                                                                                                               |
| `json-schema-edit-mode.el`       | Minor mode, entry point                                                                                                                       |

## License

Apache License 2.0. See [LICENSE](LICENSE).
