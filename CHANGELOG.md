# Revision history for Churros

## 0.1.4.1 -- 2020-10-20

* Adding `processes` function that runs a set of processes over the input.
* Adding `withChurro` helper function.

## 0.1.4.0 -- 2020-10-18

* Generalising list type to Foldable/Traversible where possible (`sources`).
* Additional documentation
* Underscore variants of prelude function that specialise Async action to `()`.

## 0.1.3.0 -- 2020-10-17

* Generalised functions to have Monoid for Async action result.

## 0.1.2.0 -- 2020-10-15

* Added type families to transport class to allow for different in/out types
* Added support for unagi-chan
* Added support for bounded unagi-chan
* Updating documentation

## 0.1.1.0 -- 2020-10-15

* Transport method `flex` now returns a pair of channels to move towards support for Unagi.
* Addition of new prelude methods such as `sources`.
* Adding README.md to Hackage contents page.

## 0.1.0.3 -- 2020-10-12

* Fixed broken changelog.
* Added `sources` combinator.

## 0.1.0.2 -- 2020-10-12

* Added new prelude functions and updated to export main library.

## 0.1.0.1 -- 2020-10-12

* Updated documentation.

## 0.1.0.0 -- 2020-10-12

* First version. Released on an unsuspecting world.
