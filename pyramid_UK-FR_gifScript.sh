#!/bin/bash

for p in pics/pyramid_UK-FR_*.pdf
do
pdftoppm "$p" "${p%.*}" -png
done
