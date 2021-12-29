#!/bin/bash

for p in pics/pyramid_UK-FR_*.pdf
do
pdftoppm "$p" "${p%.pdf*}" -png
done



for file in *-1.png
do
  mv "$file" "${file/-1/}"
done