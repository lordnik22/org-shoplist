# org-shoplist
An extension to emacs for operating on org-files who provide
food-recipes. It's meant to generate shopping lists and make
eating-plans. (We talk about delicious food — nothing technical).
## Getting Started ##
- Create a recipe list
- Mark the recipes with `org-shoplist-keyword`
- Execute `M-x org-shoplist`
- Use a formatter or leave it empty
## Ingredients ##
Enclose the ingredients with `org-shoplist-ing-start-char` and
`org-shoplist-ing-end-char`. A simple example for a
ingredient: (200g Nuts), (1 Nut), (1 big Nut)

First number (amount) with optional unit, space as separator followed by any
name which can have ANY character expect `org-shoplist-ing-start-char`
or `org-shoplist-ing-end-char`.
### Unit ###
You can use any unit by default that is listed in the calc-unit-table
(`M-x calc-view-units-table`). For additional units (See [Units](#Units))
## Recipes ##
A recipe is a group of ingredients. You pretty much can write what
ever you want. Importent is that you format your ingredients
properly. (See [Ingredients](#Ingredients))

If you really need your parentheses, (See [Enclosing](#Ingredients))

Some Examples for recipes:
- As a list:
```
* TOBUY Älpämagerone
- (250ml Rahm)
- (1 Zwiebel)
- (250g Magrone)
- (250g Emmentalerkäse)
```
- As a german description:
```
* TOBUY Älpämagerone 2
Nimm (250ml Rahm) und (1 Zwiebel) vermische es mit (250g Magrone) und (250g Emmentalerkäse).
Danach 15min köcheln lassen.
```
- As a russian description: (there you have to describe the russian
units in `org-shoplist-additional-units`.)
```
* TOBUY Älpämagerone 3
Возьмите (250мл сливок) и (1 луковицу) смешайте с (250г Магрона) и (250 г сыра Эмменталь).
Затем варить 15 минут.
```
### Nested Recipes ###
(See [Explicitness](#Explicitness)). Sorry, for that redirect.

## Shopping List ##

### Format ###

### Aggregation ###
## Customization ##
### Ingredients ###
You can define them to be the same character `M-x customize-group
org-shoplist`.

### Enclosing ###

Also everything that is enclosed with
`org-shoplist-ing-start-char` or `org-shoplist-ing-end-char` and has a
space will be handled as ingredient.
### Units ###

If you want to use your perosnal units you can add
there definition to the variable `org-shoplist-additional-units`. A
re-eval of org-shoplist.el or restart of emacs may be required.

An example, adding the german equivalent of Tablespoon(tbsp):

```
(setq org-shoplist-additional-units '((tl "1 tbsp" "Teelöffel"))
;;First is the unit specifier (case-sensitive (is a symbol))
;;Second is the definition. In this example 1tl is 1tbsp.
;;Third is a Description (can be a empty String)
```

For "ground-units" (a unit that can't be expressed by a finner/lower
unit) use "one" as definition: `((myUnit "1" "*My Special unit"))`

Circular definitions lead to errors: `((myUnit "myUnit" "*My Special unit"))`

Beaware when using relative units like cups that all cups get aggregated together.

### Explicitness ###
There are two behaviors depending on `org-shoplist-explicit-keyword` (by defualt it's nil).
When nil: 
When non-nil(t): 
### Formatters ###
You can write your own formatters to format your personal shopping
list.
