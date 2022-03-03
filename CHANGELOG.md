# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:
- Add support for `calc` expressions (#140 by @nsaunders)
- Add table selector (#141 by @plurip-software)
- Update the box-shadow implementation (#88 by @vyorkin)

New features:
- Add smart constructors for generic font families (#68, #136 by @Unisay and @JordanMartinez)
- Add support for `text-direction` (#83, #137 by @vyorkin and @JordanMartinez)
- Add outline and constituent properties (#145 by @nsaunders)
- Add support for `visibility` property (#148 by @nsaunders)

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#138 by @thomashoneyman)
- Remove ending space in css output (e.g. `padding: 1 2 3 4 `) (#135 by @chexxor and @JordanMartinez)

## [v5.0.1](https://github.com/purescript-contrib/purescript-css/releases/tag/v5.0.1) - 2021-04-19

Other improvements:
- Fix warnings revealed by v0.14.1 PureScript release (#133 by @JordanMartinez)
- Install transitive dependencies which are directly imported (#133 by @thomashoneyman)

## [v5.0.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#125 by @JordanMartinez, #126 by @kl0tl)
- Changed `flex`, `flexGrow`, and `flexShrink` to use `Number` rather than `Int` for the `grow` and `shrink` values (#64 by @andywhite37)
- Refactored `FontStyle` type to be a sum type rather than a newtype and added tests (#95 by @vyorkin)
- Fixed infix levels of selector operators (#78 by @vyorkin)

New features:
- Added roles declarations to forbid unsafe coercions (#120 by @kl0tl)
- Added `cursor` (#94 by @vyorkin)
- Added `border-spacing` (#114 by @mjgpy3)
- Added `opacity` (#91 by @vyorkin)
- Added `text-overflow` (#97 by @vyorkin)
- Added a unitless size (#90 by @vyorkin)
- Added transition CSS properties and values (#106 by @vyorkin)
- Added various transformations (#103 by @vyorkin)
- Added various selector combinators (#100 by @vyorkin)

Bugfixes:
- Fixed cubic-bezier rendering (#109 by @vyorkin)

Other improvements:
- Removed `purescript-generics` dependency (#76 by @vyorkin)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#121 by @maxdeviant)

## [v4.0.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v4.0.0) - 2018-06-05

- Updated dependencies for PureScript 0.14 (@cyrogenian)

## [v3.4.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v3.4.0) - 2018-01-24

- Lifted upper bound on `purescript-colors` (@safareli)

## [v3.3.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v3.3.0) - 2017-08-28

- Updated the re-exports from the main `CSS` module to include a variety of values and types that hadn't been added after updates elsewhere

## [v3.2.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v3.2.0) - 2017-08-28

- Added `font-style` values

## [v3.1.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v3.1.0) - 2017-08-09

- Added `zIndex` property (@maackle)

## [v3.0.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v3.0.0) - 2017-04-19

- Updated for PureScript 0.11

## [v2.1.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v2.1.0) - 2017-02-07

- Added many new values and properties (@alexmingoia)
- Fixed quoting of fonts (@siegfriedweber)

## [v2.0.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v2.0.0) - 2016-11-02

- Updated for PureScript 0.10 (@chexxor)

## [v1.1.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v1.1.0) - 2016-07-01

- Fixed warnings
- Added `Eq`, `Ord` and `Generic` instances for all types

## [v1.0.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v1.0.0) - 2016-06-29

- Updated for PureScript v0.9.1 (#33 by @deamme)

## [v0.7.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.7.0) - 2016-05-22

Many new updates:

- Now uses `purescript-colours`, fixing #22 (@sharkdp)
- Added viewport units (@menelaos)
- Prefixed animation and keyframe support (@cryogenian)
- `box-sizing` (@Podlas29)
- More element selectors (@menelaos)
- `Semigroup` instance for `CSS` (@menelaos)
- `Flexbox` module (@menelaos)
- Fuller implementations for the `Background` and `Gradient` modules (@menelaos)
- String rendering fixes (@paluh)
- `white-space` (@adarqui)

## [v0.6.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.6.0) - 2016-02-22

- Added `text-align` and `overflow`

## [v0.5.2](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.5.2) - 2015-12-08

- Added top-level `CSS` module with common re-exports (@cdepillabout)

## [v0.5.1](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.5.1) - 2015-12-02

- Fixed example in `site/` (@born2defy)

## [v0.5.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.5.0) - 2015-11-20

- Updated to latest generics
- Fixed many warnings

**Note**: this release requires PureScript 0.7.6 or newer.

## [v0.4.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.4.0) - 2015-09-30

- Bumped dependencies (@zudov)

## [v0.3.1](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.3.1) - 2015-07-23

- Qualified all external imports

## [v0.3.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.3.0) - 2015-07-14

- No longer depends on missing `purescript-nonempty-arrays` version, now uses `purescript-nonempty`

## [v0.2.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.2.0) - 2015-07-08

- Updated for PureScript 0.7

## [v0.1.0](https://github.com/purescript-contrib/purescript-css/releases/tag/v0.1.0) - 2015-05-16

- Initial versioned release.
