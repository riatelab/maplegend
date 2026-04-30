# maplegend 0.6.3

## fix
* some tests are run only if interactive()==TRUE. 


# maplegend 0.6.2

## fix
* use val_big & val_dec in "cont" legends
* remove tests giving errors or warnings from functions called by 
recordGraphics() 


# maplegend 0.6.1

## fix
* use correct segment size for prop legends when asp != 1 
* use graphics.off() instead of dev.off() in tests to avoid test errors 


# maplegend 0.6.0

## fix
* use only one lwd and border value in "prop" type legends
* use box_cex in "symb" legend to fine tune spacing between symbols
* better spacing between boxes and NA box in "choro" horizontal legends
* apply box_cex to na symbols for type = "symb"
* better defaults for "symb"
* better proportional symbols display
* test for and allow one modality legends (#7)
* correct a bug of text overflow with type = "symb" and no_data = TRUE (#8)
* correct border color assignement for type "symb"

## feat 
* add a val_max arg to adjust the proportionnal symbols ma size to a specific 
value when using type = "prop"
* new legend type "choro_point" for choropleth legends on circles or squares
* new legend type "choro_line" for choropleth legends on lines
* new legend type "choro_symb" for choropleth legends on symbols
* new legend type "typo_line" for typology legend on lines.


# maplegend 0.5.0

## fix
* allow one modality legends for types "typo", "symb" and "prop_line" (#7)


# maplegend 0.4.0

## fix
Most of the functions of the package have been refactored in order to use a more 
robust behaviour and to be suitable for plots with asp != 1.

## feat
* new legend type "histo" to draw hitogram legends (#5)
* new args val_big and val_dec have been added for types already using val_rnd
to select decimal separator and big values separator (#6)

# maplegend 0.3.0

## feat
* add alpha transparency for all legends
* redraw legends when resizing the plot device


# maplegend 0.2.0

## fix 
* use 1/4 line for typical offest (as it is done in mapsf)
* better management of symbol sizes (leg = "symb")
* better management of boxes offest (leg = "typo")

## feat
* added a `NEWS.md` file to track changes to the package.
* add a link to igraph example (#2)
* add a GIS-like example (#1)
* add alpha transparency for palettes (#4)
