import { fileURLToPath } from "node:url";
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import links from "starlight-links-validator";
import hypha from "starlight-theme-hypha";

// rowtype's documentation site, published to GitHub Pages by
// .github/workflows/docs.yml. `make docs` builds it into _build/docs,
// odoc's reference from the .mli files into public/reference/ first.
//
// Every program a page shows is a whole file under examples/, built with
// the repository, and imported by the page with `?raw` into Starlight's
// <Code> -- so a page cannot show code that does not compile. A missing
// one, or a link to a page or a heading that is not there, fails the build.
const repository = fileURLToPath(new URL("..", import.meta.url));

export default defineConfig({
  site: "https://hyphatech.github.io",
  base: "/rowtype",
  outDir: `${repository}_build/docs`,
  // Pages import an example as "@examples/<file>.ml?raw".
  vite: { resolve: { alias: { "@examples": `${repository}examples` } } },
  integrations: [
    starlight({
      title: "rowtype",
      description: "SQL queries and migrations for modern OCaml.",
      logo: { src: "starlight-theme-hypha/assets/hypha.webp" },
      favicon: "/favicon-32.png",
      head: [
        { tag: "link", attrs: { rel: "icon", type: "image/png", sizes: "16x16", href: "/rowtype/favicon-16.png" } },
        { tag: "link", attrs: { rel: "apple-touch-icon", sizes: "180x180", href: "/rowtype/apple-touch-icon.png" } },
      ],
      plugins: [
        hypha({
          brand: "hypha",
          github: "hyphatech/rowtype",
          tabs: { "Start here": "rocket", Guides: "book-open", Reference: "library" },
          links: [{ label: "Hypha Stack", href: "https://hyphatech.io/stack", icon: "layers" }],
        }),
        // The reference is odoc's, in public/, which the validator cannot see.
        links({
          components: [["Feature", "href"]],
          errorOnLocalLinks: false,
          exclude: ["**/reference/rowtype*/**"],
        }),
      ],
      sidebar: [
        { label: "Start here", items: ["install", "quick-start", "why-not-caqti"] },
        {
          label: "Guides",
          items: [
            "guide/statements",
            "guide/columns",
            "guide/bulk-writes",
            "guide/errors",
            "guide/transactions",
            "guide/pools",
            "guide/checking-statements",
            "guide/sqlite",
            "guide/migrations",
            "guide/migrating-from-a-test",
            "guide/library-migrations",
          ],
        },
        { label: "Reference", items: ["reference"] },
      ],
    }),
  ],
});
