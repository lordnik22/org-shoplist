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
`org-shoplist-ing-end-char`. You can define them to be the same
character `M-x customize-group org-shoplist`.  A simple example for a
ingredient: (200g Nuts), (1 Nut), (1 big Nut)

First amount with optional unit, space as separator followed by any
name which can have ANY character expect `org-shoplist-ing-start-char`
or `org-shoplist-ing-end-char`.
### Units ###
You can use any unit by default that is listed in the calc-unit-table
(`M-x calc-view-units-table`). If you want to use your perosnal units
you can add there definition to the variable
`org-shoplist-additional-units`.

For example adding the german equivalent of a Tablespoon(tbsp):
`(setq org-shoplist-additional-units '((tl "1 tbsp" "Teelöffel"))
;;First is the unit specifier
;;Second is the definition. In this example 1tl is 1tbsp.
;;Third is a Description (can be a empty String)`

For "ground-units" (a unit that can't be expressed by a finner/lower
unit) use one as definition: `((myUnit "1" "*My Special unit"))`

Circular definitions lead to errors: `((myUnit "myUnit" "*My Special unit"))`

Beaware when using relative units like cups that all cups get aggregated together.


## Recipes ##
Example:
`
* TOBUY Älpämagerone
- (250ml Rahm)
- (1 Zwiebel)
- (250g Magrone)
- (250g Emmentalerkäse)
`
### Explicty ###
## Aggregation ##
## Customization ##
### Variables ###
### Formatters ###
You can write your own formatters.  to format your personal shopping
list.
