shinySketch
===========
  
a  small webapp to create "art" using Shiny and ggplot


Using ggplot's geom_segments to create some fun images.

The idea is simple: Get a few parameters from the user: Canvas size, number of lines, the angle of those lines and 
some color gradients. See if you can generate something that is aesthetically pleasing, using this 
and random sampling.

There are 3 files:
1. ui.R
2. server.R
3. functionsArt.R (this file is sourced from within server.R)

