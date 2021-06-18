# Markdown markup language

- Header
    - `# Header{#id .class key="a value"}` delimits a section
- Paragraph
    - `*emphasis*`
    - `**strong emphasis**`
    - `***emphasis + strong emphasis***`
    - `~~strikeout~~`
    - `^superscript^`
    - `~subscript~`
    - `| formatted line block`
    - `---` horizontal rule
    - `-` hyphen for compound words
    - `--` en-dash for ranges
    - `---` em-dash substitutes coma, colon, seminolon or parentheses to set apart a
      phrase
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
        - `[Header link]`
    - Link attributes `[Inline link](https://organization.org){#id .class key="a value"}`
    - Reference `[reference-label]: URI` <- `[Reference text][reference-label]`
- Footnote
    - `^[Inline footnote]`
    - `[^footnote]: Footnote reference` <- `[^footnote]`
- Image
    - Inline image `![Image description](image.png)`
    - Image reference `[Image reference]: image.png` <- `![Image reference]`
- List
    - `- Bulleted item`
    - `4. Numbered item` starts from 4
    - `a. Lettered item`
    - Definition list

      ```md
      Term
      : Definition
      ```

- Table
    - Pipe table

      ```md
      Table: Caption

      Left | Center  | Right
      :--- | :---: | ---:
      A | B | C
        ```

    - Grid table
- Code
    - `inline code`{.language}
    - ``inline ecode with a ` backtick``{.language}
    - ```{.language .numberLines} code block``` = ```language code block```
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
    - `<span>Raw inline code</span>{=html5}` only for html5 output
    - ```{=html5}\newline <div>Raw code block</div>``` only for html5 output

# CSS

- CSS inclusion
    - `<div style="inline: style;"/>`
    - `<style>sel { style: block; }</style>`
    - `<link rel="stylesheet" href="style.css"/>`
    - `@import "style.css";`
- CSS selectors
    - Specificity `<div style="inline: style;"/> > #id > .class [attribute]
      :pseudo-class > element ::pseudo-element` (same specificity last rule wins)
    - Overwrite specificity `property: value !important;` (prefer more specific rules)
    - Universal selector `*`, `element`, `.class`, `#id`, `[attribute]`,
      `[attribute="exact"]`, `[attribute~="whitespace"]`, `[attribute*="substring"]`,
      `[attribute^="start"]`, `[attribute$="end"]`
    - Compound selector `element.class#id[attribute="exact"]`
    - Independent selectors `element, .class, #id, [attribute="exact"]`
    - [In]direct descendant combinator `element descendant`
    - Direct child combinator `element > child`
    - General sibling combinator `element ~ sibling`
    - Adjacent sibling combinator `element + sibling`
    - UI state pseudo-class `:active`, `:checked`, `:focus`, `:hover`, `:[in]valid`
    - Doc structure pseudo-class `:first-child`, `:last-chaild`, `:nth-child(n)`,
      `:nth-child(2n)`, `:nth-child(odd|even)`, `:root` = `html`
    - Negate selector `:not(...)`
    - Pseudo-element `::first-line`, `::first-letter`, `::before`, `::after` + `content`
- Box model `margin`, `border`, `padding` and content
    - `box-sizing: content-box | border-box;` -> `width`, `height` (`margin` is never
      considered)
    - `display: block | inline | inline-block;`
    - Block element `div`
    - Inline element `span`
- Boxes, shadows and opacity
- Backgrounds and gradients
- Web fonts and typography
- Layout positioning and stacking
- CSS transforms, transitions and animations
- Flexbox
- Responsive design, media queries and fluid typography
- CSS grid

# Knitr dynamic documents using R
