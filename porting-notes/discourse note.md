# Notes on using / adapting Ocelot 

> TL;DR - i made a fork of Ocelot that might make it easier to "grab 'n' go", tho YMMV of course

The Ocelot project from CitizenNet is a really quite amazing resource which has the potential to save a development team quite a lot of effort both in coding and front-end design decisions.

I believe the intention in open sourcing it was more to serve as a template for other projects rather than as a library for general use. Which is totally fine, it's a terrific contribution either way and i could totally see why the developers wouldn't want the burden of supporting its use in outside projects too.

However, it seems to me that there's some low-hanging fruit here for the PureScript community generally to leverage the Halogen components and the styling that Ocelot offers without:

* adopting the project structure of Ocelot
* being tied to Webpack to build the CSS
* being tied to very old version of TailwindCSS

I don't know that a fork is needed or desirable, it may be sufficient to just document the steps needed to extract and use the library, but i've made an exploratory [fork][link] for myself to explore these issues. I hope that this will be useful for others too.

Specifically, i've used this fork to validate that it is possible to get the `ui-guide` to build and run using updated releases of `postcss` and `tailwindcss` and very little else.

[link]: https://github.com/afcondon/purescript-ocelot

## what is changed in this fork

In the original repo, the CSS is built using `yarn` or `npm` to run `make` which then runs `webpack` to manage the `postcss`. There are probably good reasons to do this in CitizenNet's usage but it's quite a chain to absorb if you simply want to add some widgets to a Halogen webpage.

Additionally, in the original repo, at the root-level, `parcel` was used to build and serve development builds.

I've drastically simplified things to the point where you can build and serve the `ui-guide` using only `postcss`/`tailwind` and `spago` / `purs`, all run via npm script in root directory.

## building and serving the `ui-guide` example

This is the relevant bit of the `package.json`

```  "scripts": {
    "build": "spago build --path 'ui-guide/**/*.purs' --path 'src/**/*.purs'",
    "bundle": "spago bundle-app --main Main --to dist/bundle.js",
    "build-css": "postcss css/src/index.css -o dist/cn-tailwind.css",
  },
  "devDependencies": {
    "purescript": "0.14.2",
    "spago": "^0.20.3",
    "autoprefixer": "^10.2.6",
    "cssnano": "^5.0.6",
    "postcss-cli": "^8.3.1",
    "postcss-import": "^14.0.2",
    "tailwindcss": "^2.1.4"

  },
```
So all you should need to do from a clean install (ie with `node_modules` and `output` both empty) is to run

* `yarn build-css` - which uses the two config files `postcss.config.js` and `tailwind.config.js`, now elevated to the root of the repo
* `yarn build` - which builds all the PureScript sources under both `src` and `ui-guide`
* `yarn bundle` - which bundles an app with entry point being the `Main` from `ui-guide`

...and then you can serve the three key artefacts in the dist directory with whatever `httpd` you like, or simply open the `index.html` in your browser.

## what changes were necessary to Ocelot code?

The biggest breaking change from the early version of `tailwindcss` seems to have been the 1.0 release which changed the the classes for pinning elements to the edges of their containers. Here are the [release notes](https://github.com/tailwindlabs/tailwindcss/releases?after=v1.0.0#replace-pin-with-inset) that accompanied that change. 

Changing the classes necessitated a fairly invasive change which you can see at this [commit](https://github.com/afcondon/purescript-ocelot/commit/155c862a4861ab2116daf8c4964ce15c3f3cb28e) (you can ignore my notes.txt files, the changes that matter are all in .purs Ocelot Block files.)

## a note on production readiness, CI, etc

The purpose of this fork is to demonstrate decoupling of Ocelot components from the old Tailwind release and to build without using `parcel` or `webpack`. It undoubtedly breaks lots of things that CitizenNet CI would need, but hopefully the corollary of this is something that is easier to pick up and incorporate into a new project very easily.

But you will then **need to sort out minification, scoping, SVGs, incremental builds, file watching etc** in the way that you like best.