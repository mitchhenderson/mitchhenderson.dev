---
name: mitchhenderson.dev
description: A personal site and portfolio whose colour and type are taken from its charts.
colors:
  primary: "#134a8e"
  primary-strong: "#0d3566"
  on-primary: "#ffffff"
  ink: "#0f1b2d"
  text: "#263142"
  muted: "#5b6676"
  bg: "#ffffff"
  surface: "#f4f7fb"
  surface-strong: "#e8eff8"
  border: "#dde4ee"
  paper: "#ffffff"
  hero-bg: "#134a8e"
  hero-ink: "#ffffff"
  hero-text: "#d5e2f4"
  dark-bg: "#0e1622"
  dark-surface: "#162132"
  dark-surface-strong: "#1f2d43"
  dark-border: "#263449"
  dark-ink: "#eef2f8"
  dark-text: "#c9d2df"
  dark-muted: "#93a0b3"
  dark-primary: "#8fb8ef"
  dark-primary-strong: "#b9d3f6"
  dark-hero-bg: "#16294a"
typography:
  display:
    fontFamily: "Source Sans 3, -apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif"
    fontSize: "clamp(2.5rem, 1.3rem + 4.6vw, 4.25rem)"
    fontWeight: 700
    lineHeight: 1.02
    letterSpacing: "-0.035em"
  headline:
    fontFamily: "Source Sans 3, sans-serif"
    fontSize: "clamp(1.75rem, 1.4rem + 1.4vw, 2.25rem)"
    fontWeight: 650
    lineHeight: 1.1
    letterSpacing: "-0.025em"
  title:
    fontFamily: "Source Sans 3, sans-serif"
    fontSize: "1.625rem"
    fontWeight: 650
    lineHeight: 1.2
    letterSpacing: "-0.02em"
  body:
    fontFamily: "Source Sans 3, sans-serif"
    fontSize: "1.125rem"
    fontWeight: 400
    lineHeight: 1.6
  label:
    fontFamily: "Source Sans 3, sans-serif"
    fontSize: "0.875rem"
    fontWeight: 500
    lineHeight: 1.4
  code:
    fontFamily: "Source Code Pro, SFMono-Regular, Menlo, Consolas, monospace"
    fontSize: "0.8125rem"
    lineHeight: 1.6
rounded:
  default: "0.5rem"
  lg: "0.75rem"
  xl: "1.25rem"
  pill: "999px"
spacing:
  "1": "0.25rem"
  "2": "0.5rem"
  "3": "0.75rem"
  "4": "1rem"
  "6": "1.5rem"
  "8": "2rem"
  "12": "3rem"
  "16": "4rem"
components:
  button-primary:
    backgroundColor: "{colors.primary}"
    textColor: "{colors.on-primary}"
    rounded: "{rounded.default}"
    padding: "0.75rem 1.5rem"
    height: "2.75rem"
  button-primary-hover:
    backgroundColor: "{colors.primary-strong}"
  button-ghost:
    backgroundColor: "transparent"
    textColor: "{colors.ink}"
    rounded: "{rounded.default}"
    padding: "0.75rem 1.5rem"
  button-ghost-hover:
    backgroundColor: "{colors.surface}"
  tag:
    backgroundColor: "{colors.surface-strong}"
    textColor: "{colors.primary-strong}"
    rounded: "{rounded.pill}"
    padding: "0.25rem 0.75rem"
  feature-figure:
    backgroundColor: "{colors.paper}"
    rounded: "{rounded.lg}"
  card:
    backgroundColor: "{colors.bg}"
    rounded: "{rounded.lg}"
    padding: "0 1.5rem 1.5rem"
---

# Design System: mitchhenderson.dev

This records the system as built in `theme.scss` and `theme-dark.scss`. Where a value here and the code disagree, the code is right and this file is stale.

## Overview

**Creative North Star: "The chart's own page"** (a working name; change it freely)

The site takes its colour and its typeface from the charts it publishes. The navy is the navy in the figures, and Source Sans 3 was chosen because it is the open sibling of Myriad Pro, which the charts are set in. Figures and prose are meant to read as one design, so a chart never looks pasted into someone else's page.

The layouts are conventional on purpose: a hero with a portrait, selected work, a resume-like About page, articles with a contents list. The finish is where the effort goes. The benchmark is polished product-company sites: precise type, generous space, restrained colour and very little motion.

**Key Characteristics:**

- One colour (navy) and one type family carry the whole site.
- The work leads: charts are shown large and whole, never cropped into thumbnails.
- Answer first. Posts open with the short answer; code is folded until asked for.
- Quiet by default. Borders and whitespace separate things; shadows and motion respond to the reader.

## Colors

A single navy with neutrals tinted towards it. Nothing is pure grey.

### Primary

- **Chart Navy** (`#134a8e`): links, primary buttons, the selected state of a control, the timeline nodes, and the full-width hero band on Home. It is the line colour in the published charts.
- **Deep Navy** (`#0d3566`): hover on primary surfaces and the text in tags.

### Neutral

- **Ink** (`#0f1b2d`): headings and anything that must read first.
- **Text** (`#263142`): body copy.
- **Muted** (`#5b6676`): dates, captions, secondary labels, inactive controls.
- **Surface** (`#f4f7fb`) and **Strong Surface** (`#e8eff8`): callouts, code blocks, hover fills, tags.
- **Border** (`#dde4ee`): every rule and outline.
- **Paper** (`#ffffff`): the surface charts and `gt` tables sit on, in both themes.

### Dark theme

The same rules with a second set of values (`theme-dark.scss`): page `#0e1622`, surfaces `#162132` and `#1f2d43`, borders softened to `#263449`, and the navy lightened and desaturated to `#8fb8ef` so it doesn't glare. The hero band steps up to `#16294a` instead of switching to full navy.

### Named Rules

**The Paper Rule.** Charts and tables were drawn for a white page. In dark mode they sit on a white sheet; they are never inverted or recoloured.

**The One Colour Rule.** Navy is the only hue in the interface. Red and green appear only inside charts, where they carry data.

## Typography

**Display and body:** Source Sans 3 (system sans as fallback), self-hosted as a variable font.
**Code:** Source Code Pro.

**Character:** one humanist sans doing every job, with hierarchy made from size, weight and colour.

### Hierarchy

- **Display** (700, `clamp(2.5rem, 1.3rem + 4.6vw, 4.25rem)`, 1.02, tracking -0.035em): the Home headline only.
- **Page title** (650, `clamp(2rem, 1.4rem + 2.6vw, 3rem)`, 1.1): the `h1` on About, Posts and each post.
- **Headline** (650, `clamp(1.75rem, 1.4rem + 1.4vw, 2.25rem)`, 1.1): section headings on Home.
- **Title** (650, 1.625rem, 1.2): section headings elsewhere and the titles of featured work.
- **Lead** (400, 1.3125rem, 1.45): the opening paragraph of a page and the short answer at the top of a post.
- **Body** (400, 1.125rem, 1.6): prose, at about 700px wide.
- **UI** (1rem) and **Label** (0.875rem): controls, metadata, captions.

### Named Rules

**The Tight Headline Rule.** Large text is tightened (tracking between -0.015em and -0.035em, line-height 1.02 to 1.2) and balanced with `text-wrap: balance`. Body text is never tightened.

**The Real Heading Rule.** A section heading is a heading at heading size. Small uppercase labels are kept for interface furniture ("On this page", "tl;dr"), never used above or in place of a heading.

## Layout

Pages sit in a centred column: 62rem on Home and Posts, 50rem on About, and about 700px of prose on posts with the contents in the right margin. Spacing comes from a 4-point scale (`--s-1` to `--s-16`); sections on Home are separated by 4rem, with more space above a heading than below it.

On Home, the hero is a full-width navy band and the two analyses are large rows that alternate sides, chart in the wide column. Below 992px everything stacks into one column and the portrait shrinks to a small square above the headline.

On posts, charts and tables run 4rem wider than the text on each side at 1400px and above, and the contents list moves out by the same amount so the two never overlap. Below 992px the margin contents are replaced by a collapsible list under the short answer.

## Elevation & Depth

Flat at rest. Structure comes from 1px borders and whitespace. Shadows are a response: a featured chart and a card lift slightly on hover. The portrait in the hero is the one element with a standing shadow. In dark mode depth comes from lighter surfaces, and both shadows are switched off.

### Shadow Vocabulary

- **Card** (`0 1px 2px rgba(15, 27, 45, 0.04), 0 8px 24px rgba(15, 27, 45, 0.06)`): a chart panel at rest, a card on hover.
- **Raised** (`0 2px 4px rgba(15, 27, 45, 0.05), 0 18px 40px -8px rgba(15, 27, 45, 0.14)`): a featured chart on hover.

## Shapes

Soft rectangles. Controls, callouts and code blocks use 0.5rem corners; cards, chart panels and the timeline's callouts use 0.75rem; the hero portrait uses 1.25rem. Tags are full pills. Outlines are always 1px in the border colour. There are no thick accent borders on any side of any element.

## Components

### Buttons

- **Shape:** 0.5rem corners, at least 2.75rem tall, horizontal padding double the vertical.
- **Primary:** navy fill, white text. On the hero band it inverts to white fill with navy text.
- **Ghost:** transparent with a 1px border; fills with Surface on hover.
- **States:** hover changes fill, pressed moves down 1px, focus shows a 2px navy outline offset by 3px.

### Tags

Pills in Strong Surface with Deep Navy text, 0.875rem. They label methods and languages and are not interactive.

### Featured work (Home)

A two-column row: the chart, whole and uncropped, in a bordered panel on Paper, beside a title, a description, tags and a "Read the analysis" cue. The whole row is one link. On hover the panel lifts 3px, the title turns navy and the arrow moves right.

### Cards (Posts page, end of each post)

A bordered card with the chart across the top and the title, description and date below. The whole card is the link.

### Language switch (posts)

A small segmented control under the post title: "Code in R / Python". The chosen language is filled navy. It sets every code block on the page and remembers the choice.

### Code

Folded by default behind a one-line summary that names the language and length ("R code · 24 lines"). When a tabset shows nothing but a closed fold, the whole tabset collapses to that single muted line.

### Navigation

A white bar with the name at the left and two links at the right. The current page is filled with Strong Surface. On small screens the links collapse behind a menu button.

### Timeline (About)

Dates at the left, a line with a node per employer, and a bordered callout for each. The current role's node is filled. Printed, the page becomes a resume: site chrome, the portrait and the download button are removed.

## Do's and Don'ts

### Do:

- **Do** take colours from the tokens in `theme.scss` (`--c-*`); component rules never use literal colours.
- **Do** keep spacing on the 4-point scale (`--s-*`).
- **Do** show charts whole, on Paper, with real alt text.
- **Do** put any motion behind `prefers-reduced-motion`, and keep it to one authored moment per page plus feedback on controls.
- **Do** give every interactive element hover, pressed and focus states.

### Don't:

- **Don't** add a second accent colour or a second type family.
- **Don't** put a small uppercase label above a heading, or use one in place of a heading.
- **Don't** build a section from same-size boxes of heading plus text.
- **Don't** invert or recolour charts and tables for dark mode.
