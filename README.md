# org-shoplist
An extension to emacs for operating on org-files who provide
food-recipes. It's meant to generate shopping lists and make
eating-plans. (We talk about delicious food — nothing technical).
## Getting Started ##
- Create a recipe list (See [Ingredients](#Ingredients) and [Recipes](#Recipes))
- Set `org-shoplist-keyword` to the recipes which you want to buy
- Press `M-x org-shoplist ENT ENT`
- If you want a "TODO"-list choose as formatter:
  `org-shoplist-shoplist-as-todo-list`
## Ingredients ##
Enclose the ingredients with `org-shoplist-ing-start-char` and
`org-shoplist-ing-end-char`. Simple examples for ingredients: 
`(200g nuts), (1 nut), (1 big nut)`

Structure:
1. `org-shoplist-ing-start-char` (default: '(')
2. Number (amount)
2. optional unit
3. space as separator
4. followed by any name which can have ANY character expect `org-shoplist-ing-start-char` or `org-shoplist-ing-end-char`.
5. `org-shoplist-ing-end-char` (default: ')')
### Unit ###
You can use any unit by default that is listed in the calc-unit-table
(`M-x calc-view-units-table`). For additional units (See [Personal Units](#Personal-Units))
### Customization ###
#### Enclosing ####
Also everything that is enclosed with `org-shoplist-ing-start-char` or
`org-shoplist-ing-end-char` and has a space will be handled as
ingredient. `org-shoplist-ing-start-char` and
`org-shoplist-ing-end-char` can be the same char or they could even be
string.
#### Personal Units ####
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

Beware when using relative units like cups that all cups get aggregated together.


## Recipes ##
A recipe is a group of ingredients. You pretty much can write what
ever you want. Important is that you format your ingredients
properly. (See [Ingredients](#Ingredients))

If you really need your parentheses, (See [Enclosing](#Enclosing))

A "marked recipe" is a org-header with the `org-shoplist-keyword`. 
Example: `* TOBUY Älpämagerone`

Some Examples of recipes:
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
- As a russian description: (there you have to add the russian units
in `org-shoplist-additional-units`.)
```
* TOBUY Älpämagerone 3
Возьмите (250мл сливок) и (1 луковицу) смешайте с (250г Магрона) и (250 г сыра Эмменталь).
Затем варить 15 минут.
```
You can also have nested headers (See [Explicitness](#Explicitness)).

### Customization ###
#### Explicitness ####
There are two behaviors depending on `org-shoplist-explicit-keyword`
(by defualt it's nil). 

When nil, all ingredients of nested headers are included. 

```
* TOBUY Älpämagerone
- (250g Magrone)
** Part 1
- (1 Zwiebel)
** Part 2
- (250ml Rahm)
- (250g Emmentalerkäse) 
``` 

When non-nil, only the headers with the `org-shoplist-keyword` are
included in the shopping list. In this example we have a cream with
flakes and chocolate but without vanilla.

```
* TOBUY Cream
- (250g flakes)
** TOBUY with chocolate
- (100g chocolate
** with vanilla
- (250ml vanilla)
```
## Shopping List ##
A shopping list is a collection of ingredients, collected from the
marked recipes.

You can generate a shopping list by pressing `M-x org-shoplist RET
RET` in your org-file where recipes with the `org-shoplist-keyword` are
present.
### Customization ###
#### Aggregation ####
You can turn off aggregation by setting `org-shoplist-aggregate` to
nil.
#### Format ####
The format is established by a function which takes a shopping list as it's
arguments. The defualt format is defined by
`org-shoplist-default-format`. If you are interested in writing your
own shoppinglist-formating-function, you may find it helpful reading the
functions `org-shoplist-shoplist-as-table` or
`org-shoplist-shoplist-as-todo-list` in org-shoplist.el. After evaling
your function you can inject them when calling `org-shoplist`.
## Other Customization ##
Press `M-x customize-group org-shoplist ENT` for all custom variables.
