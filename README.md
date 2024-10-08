# org-shoplist
An extension to emacs for operating on org-files who provide
food-recipes. It's meant to generate shopping lists and make
eating-plans (delicious food — nothing technical).
## Getting Started ##
### Installation ###
Melpa: `M-x package-install ENT org-shoplist ENT`


Use-package with straight: `M-x straight-use-package ENT org-shoplist ENT`
```emacs-lisp
(use-package org-shoplist
  :straight t)
```

Or directly from source

```emacs-lisp
(use-package org-shoplist
  :straight (org-shoplist :type git :host github :repo "lordnik22/org-shoplist"))
```

Alternatively you can clone this project and add it to the load-path
in your init-file:
```
(add-to-list 'load-path "~/.emacs.d/elisp/org-shoplist")
(require 'org-shoplist)
```

### Your first Shoplist ###
- Open one of your `org-agenda-files` (can be customized by `org-shoplist-files`)
- Add a recipe (see [Recipes](#Recipes) for some examples)
- Mark your recipe (org-headers) for buying with `TOBUY` (see [Recipe Detection](#Recipe Detection)).
- Press `M-x org-shoplist ENT`

## Contribution ##
If you arise any problems or limitations which makes this package
useless to you, please leave an issue with your concern.

For any code-contributions read [CONTRIBUTING](CONTRIBUTING.md).

## Introduction ##

`org-shoplist` is a emacs package which extends org-mode with a parser
for food related structures. These include ingredients, recipes and
shopping lists. It provides general functions to create, read,
aggregate and calculate with these structures.

A normal user would only write correctly formatted food-recipes and
use the functions `org-shoplist`, `org-shoplist-factor-up` and
`org-shoplist-factor-down`.

For tinkering users, the following chapters describe these structures
in more detail, explaing what they are and which custom variable
affect there behaivior.

For programmers, read the code it's all there.

## Ingredients ##
Enclose the ingredients with `org-shoplist-ing-start-char` and
`org-shoplist-ing-end-char`. Simple examples for ingredients:
`(#200g nuts#), (#1 nut#), (#1 big nut#)`

Structure:
1. `org-shoplist-ing-start-char` (default: '(#')
2. Number (amount)
2. optional unit
3. space as separator
4. followed by any name which can have ANY character expect `org-shoplist-ing-start-char` or `org-shoplist-ing-end-char`.
5. `org-shoplist-ing-end-char` (default: '#)')
### Unit ###
For calculating with units the calc-package is used. You can use any
unit by default that is listed in the calc-unit-table (`M-x
calc-view-units-table ENT`).

For additional units:
- If you are lazy just set (`M-x customize-variable ENT
org-shoplist-auto-add-unit`)
- else see [Personal Units](#Personal-Units)
### Customization ###
#### Enclosing ####
Also everything that is enclosed with `org-shoplist-ing-start-char` or
`org-shoplist-ing-end-char` and has a space will be handled as
ingredient. `org-shoplist-ing-start-char` and
`org-shoplist-ing-end-char` can be the same char or they could even be
string.
#### Personal Units ####
Unfortunately a unit can't be any character. You are well advised to
just use A-Z and a-z. You can add the definition of your personal
units to the variable `org-shoplist-additional-units`.

An example, adding the german equivalent of Tablespoon(tbsp):
```
(setq org-shoplist-additional-units '((tl "1 tbsp" "Teelöffel"))
;;First is the unit specifier (case-sensitive (is a symbol))
;;Second is the definition. In this example 1tl is 1tbsp.
;;Third is a Description (can be a empty String)
```

For "ground-units" (a unit that can't be expressed by a finner/lower
unit) use nil as definition: `((myUnit nil "*My Special unit"))` or
just let org-shoplist add these definitions for you by setting `org-shoplist-auto-add-unit`.

Circular definitions lead to errors: `((myUnit "myUnit" "*My Special unit"))`

Beware when using relative units like cups, that all cups get
aggregated together when `org-shoplist-aggregate` is non-nil (default).

## Recipes ##
A recipe is a group of ingredients. You pretty much can write what
ever you want. Important is that you format your ingredients
properly (see [Ingredients](#Ingredients)).

You can change the enclosing of the ingredients, (see [Enclosing](#Enclosing)).

A "marked recipe" is a org-header which will be detected by `org-shoplist`. How they are detected is defined by `org-shoplist-search-type`(default: (keyword "TOBUY"), see [Recipe Detection](#Recipe Detection)).
Example using the default: `* TOBUY Älpämagerone`

### Example ###
- As a list:
```
* TOBUY Älpämagerone
- (#250ml Rahm#)
- (#1 Zwiebel#)
- (#250g Magrone#)
- (#250g Emmentalerkäse#)
```
- As a german description:
```
* TOBUY Älpämagerone 2
Nimm (#250ml Rahm#) und (#1 Zwiebel#) vermische es mit (#250g Magrone#) und (#250g Emmentalerkäse#).
Danach 15min köcheln lassen.
```
You can also have nested headers (see [Explicitness](#Explicitness)).

#### Factor ####
A recipe can have a factor-property. With the factor-property you can
define the amount of people or portion the recipe is written for. The
property is controllable with `(org-shoplist-factor-up)` and
`(org-shoplist-factor-down)`. This property affects not only
ingredients of the current header-level but also all ingrdients in the
underlying tree. Headers with no ingredients aren't affected and
[Explicitness](#Explicitness) is ignored for these functions.

Example:
```
* TOBUY Älpämagerone
  :PROPERTIES:
  :FACTOR:   2
  :END:
- (#500ml Rahm#)
- (#2 Zwiebel#)
- (#500g Magrone#)
- (#500g Emmentalerkäse#)
```

### Customization ###
`org-shoplist-inital-factor` defines the default factor when no
factor-property is set on the header.

With `org-shoplist-factor-property-name` you can define the property-name to
your taste.

## Shopping List ##
A shopping list is a collection of ingredients, generated from the
marked recipes. The marked recipes are read from `org-shoplist-files`
which by default is the same as `org-agenda-files`.

You can generate a shopping list by pressing `M-x org-shoplist RET` in
your org-file where recipes with the `org-shoplist-keyword` are
present.

With `C-u M-x org-shoplist ENT` you can pass an other
formatter-function, also see [Format](#Format).
### Customization ###
#### Recipe Detection ####

The custom variable `org-shoplist-search-type` defines how recipes are
detected. Default is `'(keyword "TOBUY")` because of legacy.


There exist three strategies:
- `'keyword`: Detect org-headers for buying which are marked with specified keyword.
- `'tag`: Detect org-headers for buying which have the specified tag plus
  are marked with one not-done-keyword.
- `'keyword+tag`: Detect org-headers for buying which are marked with
  specified keyword plus have the specified tag.

#### Explicitness ####
There are two behaviors depending on `org-shoplist-explicit-keyword`
(by defualt it's nil).

When nil, all ingredients of nested headers are included.
```
* TOBUY Älpämagerone
- (#250g Magrone#)
** Part 1
- (#1 Zwiebel#)
** Part 2
- (#250ml Rahm#)
- (#250g Emmentalerkäse#)
```

When non-nil, only the headers with the `org-shoplist-keyword` are
included in the shopping list. In this example we have a cream with
flakes and chocolate but without vanilla.

```
* TOBUY Cream
- (#250g flakes#)
** TOBUY with chocolate
- (#100g chocolate#)
** with vanilla
- (#250ml vanilla#)
```
#### Aggregation ####
You can turn off aggregation by setting `org-shoplist-aggregate` to
nil.
#### Format ####
The format is established by a function which takes a shopping list as
it's arguments. The defualt format is defined by
`org-shoplist-default-format`. If you are interested in writing your
own shoppinglist-formating-function, you may find it helpful reading
the built-in formatter-functions in `org-shoplist.el`.

Following formats are provided in this package:
- `org-shoplist-shoplist-as-table`
- `org-shoplist-shoplist-as-todo-list`
- `org-shoplist-shoplist-as-recipe-list`

You can try them out by calling `org-shoplist` with `C-u`.

Example with `org-shoplist-shoplist-as-recipe-list`:
```
C-u M-x org-shoplist ENT org-shoplist-shoplist-as-recipe-list ENT
```
## Other Customization ##
Press `M-x customize-group org-shoplist ENT` for all custom variables.
