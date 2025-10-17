# maplegend 0.4.0

# fix
Most of the functions of the package have been refactored in order to use a more 
robust behaviour and to be suitable for plots with asp != 1.

## feat
* new map type "histo" to draw hitogram legends (#5)
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
