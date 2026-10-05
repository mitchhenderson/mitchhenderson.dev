// Runs after `quarto render` (see project: post-render in _quarto.yml) and
// works on the built site only; nothing in the source tree is changed.
//
//   1. Strips unused rules from the Bootstrap bundles Quarto ships, and cuts
//      the icon font down to the icons in use.
//   2. Gives every local <img> its dimensions and lazy-loads all but the first.
//   3. Serves a WebP copy of each raster image, with the original as fallback.
//   4. Defers the scripts in the <head> so they don't hold up the first paint.
//   5. Puts a skip-to-content link first in every page.
//   6. On posts: labels each code fold with its language and length, adds a
//      reading time, the page-level R / Python switch and a contents list
//      for small screens, and closes with cards for the other current
//      analyses.
//
// Every step is safe to run twice, because Quarto also calls this after
// rendering a single page.

import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import { copyFile, mkdir, readFile, readdir, rm, stat, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { PurgeCSS } from "purgecss";
import sharp from "sharp";
import subsetFont from "subset-font";

const projectDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const siteDir = path.resolve(projectDir, process.env.QUARTO_PROJECT_OUTPUT_DIR ?? "_site");
// Converted images are kept between renders, keyed by the source file's hash
const cacheDir = path.join(projectDir, ".quarto", "post-render-cache");

const RASTER = new Set([".png", ".jpg", ".jpeg", ".gif"]);
// Below this size a second request costs more than the bytes saved
const MIN_BYTES_FOR_WEBP = 8 * 1024;
// Anything this narrow is a logo or icon, not a figure
const ICON_MAX_WIDTH = 64;
// The pixel limit guards against hostile uploads. These are the site's own
// files, and a long high-resolution animation exceeds it.
const TRUSTED = { limitInputPixels: false };
// First thing in the page for keyboard and screen reader users. Quarto's
// include-before-body lands after the site header, which is too late.
const SKIP_LINK = '<a class="skip-link" href="#quarto-document-content">Skip to content</a>';

async function walk(dir, extensions, found = []) {
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      await walk(full, extensions, found);
    } else if (extensions.has(path.extname(entry.name).toLowerCase())) {
      found.push(full);
    }
  }
  return found;
}

const kb = (bytes) => `${Math.round(bytes / 1024)} KB`;

// ---------- 1. CSS ----------

// Tag names, attribute names and attribute values from a page's markup:
// everything a selector could match, and none of the page's text
function markupNames(html) {
  const names = new Set();
  for (const [, tag, attributes] of html.matchAll(/<([a-zA-Z][\w-]*)((?:\s+[^\s"'<>\/=]+(?:\s*=\s*(?:"[^"]*"|'[^']*'|[^\s"'>]+))?)*)\s*\/?>/g)) {
    names.add(tag.toLowerCase());
    for (const [, name, double, single, bare] of attributes.matchAll(/([^\s"'<>\/=]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s"'>]+)))?/g)) {
      names.add(name);
      for (const value of (double ?? single ?? bare ?? "").split(/\s+/)) {
        if (value) names.add(value);
      }
    }
  }
  return [...names];
}

async function purgeCss() {
  const bootstrapDir = path.join(siteDir, "site_libs", "bootstrap");
  if (!existsSync(bootstrapDir)) return;

  const cssFiles = (await readdir(bootstrapDir))
    .filter((name) => name.endsWith(".css"))
    .map((name) => path.join(bootstrapDir, name));

  const results = await new PurgeCSS().purge({
    // Scripts count as content, so class names they add at runtime are kept
    content: [`${siteDir}/**/*.html`, `${siteDir}/site_libs/**/*.js`],
    css: cssFiles,
    // Posts hold thousands of words of code and prose. Reading only the
    // markup stops ordinary words ("table", "row") from keeping unused rules.
    extractors: [{ extensions: ["html"], extractor: markupNames }],
    safelist: {
      standard: ["show", "active", "fade", "collapse", "collapsing", "collapsed", "disabled", "quarto-light", "quarto-dark"],
      // Third-party widgets, and names scripts build up from string pieces
      greedy: [/^tippy/, /^gt_/, /^giscus/, /^modal/, /^dropdown/, /data-bs-/, /data-mode/],
    },
  });

  for (const { file, css } of results) {
    const before = (await stat(file)).size;
    await writeFile(file, css);
    console.log(`css    ${path.basename(file)}: ${kb(before)} -> ${kb(Buffer.byteLength(css))}`);
  }

  await subsetIconFont(bootstrapDir);
}

// The icon font holds about two thousand glyphs and the site uses a handful.
// Once the unused icon rules are gone, the rules that remain say which
// glyphs to keep.
async function subsetIconFont(bootstrapDir) {
  const cssFile = path.join(bootstrapDir, "bootstrap-icons.css");
  const fontFile = path.join(bootstrapDir, "bootstrap-icons.woff");
  const subsetName = "bootstrap-icons.subset.woff2";
  if (!existsSync(cssFile) || !existsSync(fontFile)) return;

  const css = await readFile(cssFile, "utf8");
  if (css.includes(subsetName)) return;

  const glyphs = [...css.matchAll(/content:\s*"\\([0-9a-f]{3,6})"/gi)]
    .map((match) => String.fromCodePoint(parseInt(match[1], 16)))
    .join("");
  if (!glyphs) return;

  const subset = await subsetFont(await readFile(fontFile), glyphs, { targetFormat: "woff2" });
  await writeFile(path.join(bootstrapDir, subsetName), subset);
  await writeFile(cssFile, css.replace(/src:\s*url\([^)]*\)\s*format\("woff"\)/, `src: url("./${subsetName}") format("woff2")`));
  console.log(`font   bootstrap-icons: ${kb((await stat(fontFile)).size)} -> ${subset.length} bytes (${[...glyphs].length} glyphs)`);
}

// ---------- Scripts ----------

// Quarto loads its libraries in the <head> without defer, which holds up the
// first paint. Deferred scripts still run in order and before
// DOMContentLoaded, which is when Quarto's inline code first uses these.
//
// Only libraries checked to be used that late are listed. Others (the
// lightbox, for one) are called by inline code straight away, and deferring
// them breaks the feature.
const DEFERRABLE = /\/(?:bootstrap\.min|popper\.min|tippy\.umd\.min|clipboard\.min|anchor\.min|quarto-nav)\.js"/;

function deferHeadScripts(html) {
  const end = html.indexOf("</head>");
  if (end < 0) return html;
  const head = html.slice(0, end).replace(/<script\b([^>]*\bsrc="[^"]*"[^>]*)>/gi, (tag, attributes) =>
    !DEFERRABLE.test(attributes) || /\b(defer|async)\b|type="module"/i.test(attributes) ? tag : `<script${attributes} defer>`,
  );
  return head + html.slice(end);
}

// ---------- Posts ----------

const LANGUAGE_NAMES = { r: "R", python: "Python", stan: "Stan", sql: "SQL", bash: "Shell" };
const WORDS_PER_MINUTE = 230;

// "Code" on a closed fold says nothing about what is inside. Name the
// language and the length so a reader can decide whether to open it.
function labelCodeFolds(html) {
  return html.replace(/(<details class="code-fold">\s*<summary>)Code(<\/summary>)([\s\S]*?)(?=<\/details>)/g, (whole, open, close, body) => {
    const language = body.match(/<pre class="sourceCode (\w+)/)?.[1];
    const lines = (body.match(/<span id="cb\d+-\d+"/g) ?? []).length;
    if (!language || lines === 0) return whole;
    const name = LANGUAGE_NAMES[language] ?? language;
    return `${open}${name} code · ${lines} ${lines === 1 ? "line" : "lines"}${close}${body}`;
  });
}

// Reading time for the prose only; code and its output are skipped
function addReadingTime(html) {
  if (html.includes('class="reading-time"')) return html;
  const main = html.match(/<main\b[^>]*>([\s\S]*?)<\/main>/)?.[1];
  if (!main) return html;

  const prose = main
    .replace(/<(pre|script|style|table)\b[\s\S]*?<\/\1>/g, " ")
    .replace(/<[^>]+>/g, " ")
    .replace(/&[a-z0-9#]+;/gi, " ");
  const minutes = Math.max(1, Math.round(prose.split(/\s+/).filter(Boolean).length / WORDS_PER_MINUTE));
  const item = `<div><div class="quarto-title-meta-heading">Reading time</div><div class="quarto-title-meta-contents"><p class="reading-time">${minutes} min read</p></div></div>`;

  return html.replace(/(<div class="quarto-title-meta">[\s\S]*?)(<\/div>\s*<\/header>)/, `$1${item}$2`);
}

// The cards on the Posts page, with links made absolute so they work from
// any post
async function readWorkCards() {
  const listing = path.join(siteDir, "posts.html");
  if (!existsSync(listing)) return [];
  const html = await readFile(listing, "utf8");

  return [...html.matchAll(/<div class="work-card"[^>]*>[\s\S]*?<\/div>/g)].map(([card]) => {
    const portable = card
      // Undo this script's own image handling from an earlier run
      .replace(/<picture>(?:\s*<source\b[^>]*>)*\s*(<img\b[^>]*>)\s*<\/picture>/g, "$1")
      .replace(/ data-[\w-]+="[^"]*"/g, "")
      .replace(/\b(href|src)="\.\//g, '$1="/');
    return { href: portable.match(/href="([^"]+)"/)?.[1], card: portable };
  });
}

// Ends each post by pointing to the other current analyses
function addMoreWork(html, htmlFile, cards) {
  if (html.includes('class="more-work"')) return html;
  const self = `/${path.relative(siteDir, htmlFile).split(path.sep).join("/")}`;
  const others = cards.filter(({ href }) => href && href !== self);
  if (others.length === 0) return html;

  const section = `<section class="more-work"><h2 class="section-title">More work</h2><div class="work-grid">${others.map(({ card }) => card).join("")}</div></section>`;
  // Above the comments where a post has them, otherwise at the end
  const comments = html.indexOf('<input type="hidden" id="giscus-base-theme"');
  const at = comments >= 0 ? comments : html.lastIndexOf("</main>");
  return at < 0 ? html : html.slice(0, at) + section + html.slice(at);
}

// The two controls below are written into the page here, not built in the
// browser, so the article doesn't jump down when they appear.
// posts/post-enhancements.html only wires up their behaviour.

// One R / Python switch for the whole page, under the title
function addLanguageSwitch(html) {
  if (!html.includes('data-group="language"') || html.includes('class="language-switch"')) return html;
  const start = html.indexOf('<header id="title-block-header"');
  const end = start < 0 ? -1 : html.indexOf("</header>", start);
  if (end < 0) return html;

  const control =
    '<div class="language-switch" role="group" aria-label="Code language">' +
    '<span class="language-switch-label">Code in</span>' +
    '<button type="button" data-language="r" aria-pressed="true">R</button>' +
    '<button type="button" data-language="python" aria-pressed="false">Python</button>' +
    "</div>";
  return html.slice(0, end) + control + html.slice(end);
}

// Index just past the </div> that closes the <div> opening at `start`
function endOfDiv(html, start) {
  const tags = /<(\/?)div\b[^>]*>/g;
  tags.lastIndex = start;
  let depth = 0;
  for (let tag = tags.exec(html); tag; tag = tags.exec(html)) {
    depth += tag[1] ? -1 : 1;
    if (depth === 0) return tags.lastIndex;
  }
  return -1;
}

// A collapsible "On this page" for screens too narrow to show the contents
// in the margin. It sits under the short answer when a post opens with one.
function addInlineContents(html) {
  if (html.includes('class="toc-inline"')) return html;
  const nav = html.match(/<nav id="TOC"[^>]*>([\s\S]*?)<\/nav>/)?.[1];
  const headerStart = html.indexOf('<header id="title-block-header"');
  if (!nav || headerStart < 0) return html;

  const list = nav
    .slice(nav.indexOf("<ul"), nav.lastIndexOf("</ul>") + 5)
    .replace(/ (?:id|class|data-scroll-target)="[^"]*"/g, "");
  const details = `<details class="toc-inline"><summary>On this page</summary>${list}</details>`;

  let at = html.indexOf("</header>", headerStart) + "</header>".length;
  const callout = html.slice(at).match(/^\s*<div\b[^>]*\bclass="[^"]*\bcallout\b/);
  if (callout) {
    const end = endOfDiv(html, at + callout[0].indexOf("<div"));
    if (end > 0) at = end;
  }
  return html.slice(0, at) + details + html.slice(at);
}

const isPost = (html) => /<body\b[^>]*\bclass="[^"]*\bpost-page\b/.test(html);

const enhancePost = (html, htmlFile, workCards) =>
  addMoreWork(addInlineContents(addLanguageSwitch(addReadingTime(labelCodeFolds(html)))), htmlFile, workCards);

// ---------- 2 and 3. Images ----------

// Resolves an <img src> to a file in the built site; null for anything remote
function resolveSource(src, htmlFile) {
  if (!src || /^(?:[a-z]+:)?\/\//i.test(src) || src.startsWith("data:")) return null;
  const clean = decodeURIComponent(src.split(/[?#]/)[0]);
  const file = clean.startsWith("/") ? path.join(siteDir, clean) : path.resolve(path.dirname(htmlFile), clean);
  return existsSync(file) ? file : null;
}

function readAttributes(tag) {
  const attributes = new Map();
  const pattern = /([^\s"'<>\/=]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s"'>]+)))?/g;
  const body = tag.replace(/^<img\b/i, "").replace(/\/?>$/, "");
  for (const match of body.matchAll(pattern)) {
    attributes.set(match[1].toLowerCase(), match[2] ?? match[3] ?? match[4] ?? null);
  }
  return attributes;
}

function writeImg(attributes) {
  const parts = [...attributes].map(([name, value]) => (value === null ? name : `${name}="${value}"`));
  return `<img ${parts.join(" ")}>`;
}

const pixels = (value) => (value !== null && /^\d+(\.\d+)?(px)?$/.test(value) ? parseFloat(value) : null);

// Fills in whichever of width and height is missing, keeping the aspect ratio
// of the file. An image with neither gets its natural size.
function setDimensions(attributes, natural) {
  const width = pixels(attributes.get("width") ?? null);
  const height = pixels(attributes.get("height") ?? null);
  if (attributes.has("width") && attributes.has("height")) return;
  // A percentage or other unit: leave the author's sizing alone
  if ((attributes.has("width") && width === null) || (attributes.has("height") && height === null)) return;

  if (width !== null) {
    attributes.set("height", String(Math.round((width * natural.height) / natural.width)));
  } else if (height !== null) {
    attributes.set("width", String(Math.round((height * natural.width) / natural.height)));
  } else {
    attributes.set("width", String(natural.width));
    attributes.set("height", String(natural.height));
  }
}

const imageInfo = new Map();

// Natural size, plus the WebP copies written beside the original
async function inspect(file) {
  if (imageInfo.has(file)) return imageInfo.get(file);

  const info = { natural: null, webp: null, still: null };
  imageInfo.set(file, info);

  let metadata;
  try {
    metadata = await sharp(file, { animated: true, ...TRUSTED }).metadata();
  } catch (error) {
    console.warn(`skip   ${path.relative(siteDir, file)}: ${error.message}`);
    return info;
  }
  // For an animation, height covers every frame stacked; pageHeight is one
  info.natural = { width: metadata.width, height: metadata.pageHeight ?? metadata.height };

  const extension = path.extname(file).toLowerCase();
  const bytes = (await stat(file)).size;
  if (!RASTER.has(extension) || bytes < MIN_BYTES_FOR_WEBP) return info;

  const animated = (metadata.pages ?? 1) > 1;
  const hash = createHash("sha1").update(await readFile(file)).digest("hex").slice(0, 16);
  const stem = file.slice(0, -extension.length);

  const convert = async (suffix, build) => {
    const cached = path.join(cacheDir, `${hash}${suffix}.webp`);
    if (!existsSync(cached)) await build().toFile(cached);
    const output = `${stem}${suffix}.webp`;
    await copyFile(cached, output);
    return output;
  };

  if (animated) {
    info.webp = await convert("", () => sharp(file, { animated: true, ...TRUSTED }).webp({ quality: 80, effort: 4 }));
    // The last frame stands in for the animation when motion is reduced
    info.still = await convert(".still", () => sharp(file, { page: metadata.pages - 1, ...TRUSTED }).webp({ nearLossless: true, quality: 60 }));
  } else if (extension === ".png") {
    // Charts are flat colour and fine text: near-lossless keeps the edges clean
    info.webp = await convert("", () => sharp(file).webp({ nearLossless: true, quality: 60, effort: 5 }));
  } else {
    info.webp = await convert("", () => sharp(file).webp({ quality: 80, effort: 5 }));
  }

  // Keep the original if the copy is no smaller. That happens with animated
  // charts: GIF is already compact on flat colour.
  if ((await stat(info.webp)).size >= bytes) {
    await rm(info.webp);
    info.webp = null;
  }
  return info;
}

// The same reference the page used, pointing at a sibling file
function siblingUrl(src, originalFile, siblingFile) {
  const from = path.basename(originalFile);
  const to = path.basename(siblingFile);
  const index = src.lastIndexOf(encodeURI(from)) >= 0 ? src.lastIndexOf(encodeURI(from)) : src.lastIndexOf(from);
  const length = src.slice(index).startsWith(encodeURI(from)) ? encodeURI(from).length : from.length;
  return src.slice(0, index) + encodeURI(to) + src.slice(index + length);
}

async function processHtml(htmlFile, workCards) {
  const original = await readFile(htmlFile, "utf8");
  const html = isPost(original) ? enhancePost(original, htmlFile, workCards) : original;
  // An <img> already inside <picture> was handled on an earlier run
  const pattern = /(<picture>(?:\s*<source\b[^>]*>)*\s*)?(<img\b[^>]*>)/gi;

  const pieces = [];
  let cursor = 0;
  let seen = 0;
  let content = 0;
  let wrapped = 0;

  for (const match of html.matchAll(pattern)) {
    const [whole, insidePicture, tag] = match;
    pieces.push(html.slice(cursor, match.index));
    cursor = match.index + whole.length;

    const attributes = readAttributes(tag);
    const src = attributes.get("src");
    const file = resolveSource(src, htmlFile);
    if (insidePicture || !file) {
      pieces.push(whole);
      continue;
    }

    const info = await inspect(file);
    if (!info.natural) {
      pieces.push(whole);
      continue;
    }

    seen += 1;
    setDimensions(attributes, info.natural);
    // Logos and icons are left exactly alike wherever they repeat: Quarto
    // syncs the R / Python tabs by comparing their markup, so one logo
    // marked differently from the rest would break that.
    const icon = path.extname(file).toLowerCase() === ".svg" || (pixels(attributes.get("width") ?? null) ?? Infinity) <= ICON_MAX_WIDTH;
    if (!icon) {
      content += 1;
      // The first image is likely in view on load, so it is fetched eagerly
      if (content > 1 && !attributes.has("loading")) attributes.set("loading", "lazy");
    }
    if (!attributes.has("decoding")) attributes.set("decoding", "async");

    const img = writeImg(attributes);
    // A srcset already offers the browser a choice of files
    if ((!info.webp && !info.still) || attributes.has("srcset")) {
      pieces.push(img);
      continue;
    }

    const sources = [];
    if (info.still) {
      sources.push(`<source media="(prefers-reduced-motion: reduce)" type="image/webp" srcset="${siblingUrl(src, file, info.still)}">`);
    }
    if (info.webp) {
      sources.push(`<source type="image/webp" srcset="${siblingUrl(src, file, info.webp)}">`);
    }
    pieces.push(`<picture>${sources.join("")}${img}</picture>`);
    wrapped += 1;
  }

  pieces.push(html.slice(cursor));
  let output = deferHeadScripts(pieces.join(""));
  if (!output.includes('class="skip-link"')) {
    output = output.replace(/<body\b[^>]*>/, (body) => body + SKIP_LINK);
  }
  if (output !== original) await writeFile(htmlFile, output);
  return { seen, wrapped };
}

// ---------- Run ----------

await mkdir(cacheDir, { recursive: true });

const htmlFiles = await walk(siteDir, new Set([".html"]));
// Read before any page is rewritten, so the cards are in their rendered form
const workCards = await readWorkCards();
let seen = 0;
let wrapped = 0;
for (const htmlFile of htmlFiles) {
  const counts = await processHtml(htmlFile, workCards);
  seen += counts.seen;
  wrapped += counts.wrapped;
}
console.log(`images ${seen} sized across ${htmlFiles.length} pages, ${wrapped} given a WebP source`);

// Last, so the pages are read in their final form: rules for markup added
// above (<picture>, the skip link, the cards) count as used
await purgeCss();
