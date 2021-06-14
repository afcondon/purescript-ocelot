# Notes on using / adapting Ocelot 

The Ocelot project from CitizenNet is a really quite amazing resource which has the potential to save a development team quite a lot of effort both in coding and front-end design decisions.

I believe the intention in open sourcing it was more to serve as a template for another project than as a library for general use. Which is totally fine, it's a terrific contribution either way and i could totally see why the developers wouldn't want the burden of supporting its use in outside projects too.

However, it seems to me that there's some low-hanging fruit here for the PureScript community generally to leverage the Halogen components and the styling that Ocelot offers without:

* adopting the project structure of Ocelot
* being tied to Webpack to build the CSS
* being tied to very old version of TailwindCSS

I don't know that a fork is needed or desirable, it may be sufficient to just document the steps needed to extract and use the library, but i've made an exploratory fork for myself to explore these issues. I hope that this will be useful for others too.

Specifically, i've used this fork to validate that it is possible to get the `ui-guide` to build and run using updated releases of `postcss` and `tailwindcss` and very little else.

In the original repo, the CSS is built using `yarn` or `npm` to run `make` which then runs `webpack` to manage the `postcss`. There are probably good reasons to do this in CitizenNet's usage but it's quite a chain to absorb 

I am now working on 


spago build --path 'ui-guide/**/*.purs' --path 'src/**/*.purs'
spago bundle-app --main Main --to dist/bundle.js