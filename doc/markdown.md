# Markdown

- Header
    - `# Header{#id .class key="a value"}`
- Paragraph
    - `*emphasis*`
    - `**strong emphasis**`
    - `***emphasis + strong emphasis***`
    - `~~strikeout~~`
    - `^superscript^`
    - `~subscript~`
    - `| formatted line block`
    - `---` horizontal rule
    - `\*` escape control character
    - `\<newline>` hard line break
    - `\<space>` non-breaking space
- Link
    - Web
        - `[Inline link](https://organization.org)`
        - `[Link reference]: https://organization.org` <- `[Link reference]`
        - Raw URI inline link `<https://organization.org>`
    - Email
        - `[Inline email](mailto:user@mail.com)`
        - `[Email reference]: user@mail.com` <- `[Email reference]`
        - Raw email inline link `<user@mail.com>`
    - Internal link
        - `<div id="internal-link"/>` <- `[Internal link](#internal-link)`
        - `<div id="internal-link"/>`, `[Internal link reference]: #internal-link`
          <- `[Internal link reference]`
    - Link attributes `[Inline link](https://organization.org){#id .class key="a value"}`
    - Reference `[reference-label]: URI` <- `[Reference text][reference-label]`
    - Footnote `[^footnote]: Content` <- `[^footnote]`
- Image
    - Inline image `![Image description](image.png)`
    - Image reference `[Image reference]: image.png` <- `![Image reference]`
- List
    - `- Bulleted item`
    - `1. Numbered item`
    - `a. Lettered item`
    - Definition list

      ```md
      Term
      : Definition
      ```

- Table
    - Pipe table

      ```md
      Left | Center  | Right
      :--- | :---: | ---:
      A | B | C
        ```

    - Grid table
- Code
    - `inline code`{.language}
    - ``inline ecode with a ` backtick``{.language}
    - ```{.language} code block``` = ```language code block```
- Quote
    - `> Block quote`
    - `> > Nested block quote`
- Math
    - `$inline tex math$`
    - `$$block tex math$$`
- Raw HTML
    - `:::{#id .class key="a value"}\newline Content \newline:::` =
      `<div id="id" class="class" key="a value">Content</div>`
    - `[Content]{#id .class key="a value"}` =
      `<span id="id" class="class" key="a value">Content</span>`
    - `<span>Raw inline code</span>{=html5}`
    - ```{=html5}<div>Raw code block</div>```
