# The Smart design system, a Haskell implementation

The [Smart design system](https://design.smart.coop/) contains a reference
implementation of the design system itself. This repository is a Haskell
implementation, intended to be used in prototyping.

This repository contains both a library and an executable. The library presents
is written with `blaze-html`. The executable is used to recreate components and
example pages.

A [GitHub
Action](https://github.com/smartcoop/design-hs/blob/main/.github/workflows/deployment.yml)
is setup to generate pages and serve them at
[design-hs.smartcoop.xyz](https://design-hs.smartcoop.xyz/).

## The structure

The code is organized as a Cabal project, with two packages:

- `design-hs-lib`: the library itself.
- `design-hs-exe`: the executable server that provides us with a page like
  https://design.smart.coop/development/docs/, but using the lib.

## Notes on ease of development.

To keep the compilation and test costs low at this point; I'll take the liberty to put the exe into the library package and we'll split this out as things mature. 

All the packages made available via this package are listed in `contents.nix`.

## Building the static site

```
$ nix-build -A design-hs-html
$ nix-shell -p busybox --run 'httpd -f -p 8000 -h result/html'
```

## Formatting with Brittany

```
$ scripts/brittany.sh
```

## Linting with HLint

```
$ scripts/hlint.sh
```
