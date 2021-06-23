# Markdown markup language

- `pandoc` options
    - HTML `pandoc -f markdown -t html5 -s --self-contained --mathml`
    - PDF `pandoc -f markdown -t pdf -s --pdf-engine wkhtmltopdf --mathml`
- Document metadata
    - `<head>` children

      ```yaml
      # -c style.css
      css:
        - style/style.css
      # -H head.html
      header-includes:
        - <link rel="shortcut icon" href="image/favicon.png"/>
      ```

- Header
    - `# Header{#id .class key="a value"}` delimits a section
- Paragraph
    - `*emphasis*`
    - `**strong emphasis**`
    - `***emphasis + strong emphasis***`
    - `~~strikeout~~`
    - `^superscript^`
    - `~subscript~`
    - `| preserves formatting`
    - `---` horizontal rule
    - `-` hyphen for compound words
    - `--` en-dash for ranges
    - `---` em-dash substitutes coma, colon, seminolon or parentheses to set apart a
      phrase
    - `\*` escape control character
    - `\<newline>` hard line break
    - `\<space>` non-breaking space
    - `<!-- comment -->`
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
    - `<span>Raw inline code</span>{=html5}` only for html5 output
    - ```{=html5}\newline <div>Raw code block</div>\newline``` only for html5 output

# CSS

- CSS inclusion
    - `<div style="inline: style;"/>`
    - `<style>sel { style: block; }</style>`
    - `<link rel="stylesheet" href="style.css"/>`
    - `@import "style.css";`
- CSS selectors
    - Specificity (form higher to lower, same specificity last rule wins)
        - Inline `<div style="inline: style;"/>`
        - Id `#id`
        - Class `.class [attribute] :pseudo-class`
        - Element `element ::pseudo-element`
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
- Box model `margin`, `border`, `padding` and `content`
    - `box-sizing: content-box | border-box;` -> `width`, `height` (`margin` is never
      considered) + `[min|max]-[width|height]`
    - `display: block | inline | inline-block;`
    - `block` respects `width` and `height`, is palced on its own line, takes up the
      full width of the container and has just enough height to fit the content
    - `inline` ignores `width` and `height`, is palced inline in the text flow, takes up
      enough width and height to fit the content
    - `inline-block` respects `width` and `height` and is palced inline in the text flow
    - `display: none;` removes element from the flow
    - `visibility: hidden` preserves element space in the flow
- CSS units
    - `px` usually used only for root `font-size`
    - `em` variable, relative to inherited `font-size`
    - `rem` constant, relative to root `font-size`
    - `wv`, `wh` relative to viewport (responsive design)
    - `%` percentage of inherited `font-size` or `width` (responsive design)
    - `cm`, `mm`, `pt` absolute units (print styling)
    - `calc(...)` unit calculation with CSS variables
- CSS colors
    - `#1a2b3c`
    - `rgb(255, 255, 255)`, `rgb(255 255 255)`
    - `rgba(255, 255, 255, 0.5)`, `rgba(255 255 255 / 0.5)`
- CSS overflow `overflow-[x|y]: visible | hiddent | scroll | auto;`
- CSS variables
    - Variable inheritance: variables cascade down to descendant elements
    - `:root { --global-variable: value; }` <- `var(--global-variable, [default])`
    - Counter `:root { counter-reset: h1c; }`, `h1:before { counter-increment: h1c;
       content: counter(h1c) ". "; }`
- Border `border`, `border-width`, `border-style`, `border-color`, `border-radius`,
  `box-shadow`, `opacity`
- Background `background`, `background-color`, `background-image`, `background-repeat`,
  `background-position`, `background-size`, `background-clip`
- Gradient `background-image: linear-gradient() | radial-gradient()`
- Text styling `font-family`, `font-size`, `color`, `font-weight`, `font-style`,
  `text-decoration`, `text-transform`, `letter-spacing`, `font-variant`, `text-shadow`
- Text layout `white-space`, `line-height`, `text-indent`, `text-overflow`,
  `text-align`, `vertical-align`
- Web font
    - Import `@font-face { font-family: "FF"; src: url("ff.woff2") format("woff2");
      font-weight: ...; font-style: ...; }`
    - Use `body { font-family: "FF", sans; }`

- Boxes, shadows and opacity
- Backgrounds and gradients
- Web fonts and typography
- Layout positioning and stacking
- CSS transforms, transitions and animations
- Flexbox
- Responsive design, media queries and fluid typography
- CSS grid

# Knitr dynamic documents using R

- Global R options
    - `options(option = value)`
- Global knitr chunk options
    - `opts_chunk$set(error = F, class.source = c(...))`
- Inline code `r ...`
- Code block ```{r label, options ...}\newline ... \newline```
- Evaluation ```{r label, eval, include, echo}```
- Plot ```{r label, dev, dev.args, fig.cap, fig.width, fig.height, fig.dim}```
- R source and output ```{r lable, class.source = c(...), class.output = c(...)}```
- Table
    - Global R options `knitr.table.format`, `knitr.kable.NA`
    - `kable(x, "pipe|html", caption, col.names, row.names, align, digits, format.args)`
    - Extra packages `gt`, `reactable`

- Output ```{r label, out.width, out.height}```

# Inkscape vector graphics

# PlantUML sequence diagrams

# Dot graph visualization
