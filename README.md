# mitchhenderson.dev

[![Render](https://github.com/mitchhenderson/mitchhenderson.dev/actions/workflows/render.yml/badge.svg)](https://github.com/mitchhenderson/mitchhenderson.dev/actions/workflows/render.yml)

Source for [mitchhenderson.dev](https://mitchhenderson.dev), where I write up analyses of practical questions in sport, with the data and code in R and Python.

[![The home page](.github/readme/home.webp)](https://mitchhenderson.dev)

## Posts

**[Making 1 rep max estimates more accurate and honest](https://mitchhenderson.dev/posts/2026-01-04-e1rm-modelling/)** ([source](posts/2026-01-04-e1rm-modelling/index.qmd))

The standard 1RM formulas treat every athlete the same. This is a Bayesian multilevel model (brms and Stan, with a PyMC version) that swaps the fixed constant in the Epley formula for a parameter learned per athlete. On held-out sets from simulated data the error was about half that of the standard formula (2.7 kg vs 5.5 kg), and you get an interval instead of a single number.

**[How many wins do NRL teams need to make the finals?](https://mitchhenderson.dev/posts/2025-01-18-how-many-wins-do-nrl-teams-need-to-make-the-finals/)** ([source](posts/2025-01-18-how-many-wins-do-nrl-teams-need-to-make-the-finals/index.qmd), [data](posts/2025-01-18-how-many-wins-do-nrl-teams-need-to-make-the-finals/2014-2024_nrl_match_results.csv))

Probably 13. Ten real seasons aren't enough to answer this, so I simulated 10,000 of them, with team strength taken from bookmaker odds and a home advantage estimated from past results.

The posts from 2020 and 2021 are older tutorials on GPS and wearable data in R. I've left them up, but they aren't how I'd do things now.

## How it's built

It's a [Quarto](https://quarto.org) site hosted on Netlify. The parts that aren't standard Quarto:

- Posts are frozen, so the site builds without R or Stan installed. The catch is that editing a post's `.qmd` does nothing until you re-render that post.
- `tools/post-render.mjs` runs after every render. It strips unused CSS (Bootstrap goes from 528 KB to 145 KB), cuts the icon font down to the six icons in use (176 KB to under 1 KB), adds image dimensions and WebP copies, and defers scripts. On posts it also adds the reading time and the R / Python switch.
- The lines behind the heading on the home page are posterior draws from a Gaussian process, drawn by `tools/hero-fan.R`. There's a Python version in `tools/hero-fan.py`.
- Colours, type and spacing are written up in [DESIGN.md](DESIGN.md).

## Running it

You need Quarto 1.9 and Node.

```bash
git clone --depth 1 https://github.com/mitchhenderson/mitchhenderson.dev.git
cd mitchhenderson.dev
npm install
quarto preview
```

Use `--depth 1`. The history has a large data file in it that has since been removed.

To re-run a post (`quarto render posts/<post>/index.qmd`) you also need R, the packages loaded at the top of that post, and:

- my helper package, for the chart captions and font setup: `remotes::install_github("mitchhenderson/mitchhenderson-R-package")`
- CmdStan (through cmdstanr) for the 1RM post
- Myriad Pro if you want the charts to match. Without it they still render, just in a fallback font.

Only the R code runs when a post is rendered. The Python is there to read and copy. The 2020 Apple Health post can't be re-run because its data export is no longer in the repo.

## Licence

Code is [MIT](LICENSE). Writing and figures are [CC BY 4.0](LICENSE-CC-BY-4.0.md), apart from Heidi Thornton's guest post, which is hers. Third-party logos, the fonts and the `details` extension keep their own terms.
