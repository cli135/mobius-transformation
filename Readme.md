Final Project - FPSE - Moebius Transformation in ASCII Art
--------------------------------


Authors:
- Hongyi Liu,
-Christopher Li

## Dependencies/Libraries used:

- `Core`
- `OUnit2`
- `Imagelib`
  - This library has been tested and is working now, with the functionality displayed below! Namely, we can read from PNG files, displaying them as ASCII art, and we can write to PNG files, displaying them the rasterizer output as normal PNG images (in print_ascii.ml)
  - The results of reading from PNG files and rendering them as ASCII art is also shown below:
  - (insert image here)

- `Yojson`

## How to run the program / Usage:

dune b
dune test
./_build/default/src/user_interface.exe
This command runs the program.
You will then see an image displayed, with the option to enter commands interactively. Here is a list of commands that are accepted syntax:

How to use the user interface:
```
How to use the user interface:
set [alpha/beta] [angle] : set alpha/beta to the input angle in degree
  set alpha 90
  set beta 180
add [alpha/beta] [angle] : increment current alpha/beta by the input 
angle in degree 
  add alpha 15
  add beta -10
view [Sphere/Planar/Orthogonal] : change render views
  view Sphere
set center [xfloat] [yfloat] [zfloat] : set the sphere center to a new 
location, zfloat must be a positive value.
  move center 0. 1. 3.
set [paramname] [paramvalue] : set all the customizable parameters for 
the viewport
  set img_w 100
  set view_size 4
  set plane_bd 4
  set half_edge_length 2
  set line_w 0.25
  set grid_size 2
  set frame_rate 30
  set duration 2.
  set supersampling true
cool : this will play a cool animation :)
reset: reset all parameters
exit: exit the program

```

## Overview:

- The purpose of this project is to create an animation of the Moebius transformation via a rasterizer.
  - Users are be able to interact with the animation by setting parameters, and the animation is displayed via ASCII art.
  - The goal is to provide an intuitive and interactive visualization of the Moebius transformation and how it is related to the stereographic projection in the command line terminal via ASCII art.

## Update notes:

- Hongyi
  - refactored the user interface extensively,
  - added the capability for supersampling in the rasterizer, and
  - wrote animations for the parameters and refactored the way the keyframe animations are done (by jointly interpolating parameters).
    - If you type 'cool' in the ./_build/default/src/user_interface.exe application, then a predefined animation will run.
  The updates to the code are in rasterizer.ml and user_interface.ml

- Christopher
  - refactored some of the user interface options
  - refactored ascii_printer.ml to be cleaner
  - wrote functions to read from and write to PNG files using the imagelib library


## Summary of Codebase in `mobius-transformation/src`:
- rasterizer.mli
- rasterizer.ml
  - This performs the rasterization of the images for the Moebius transformation.
- math.mli
- math.ml
  - This is a math library that the rasterizer.ml uses.
- ascii_printer.mli
- ascii_printer.ml
  - This contains a function to print out a list of floats as an ASCII image.
- user_interface.ml
  - This interactive executable handles the logic and syntax for the interactive user interface.
- print_ascii.mli
- print_ascii.ml
  - This interactive executable handles the reading from and writing to PNG files.
- dune
  - The dune file for this project.

## Documents
 
Our Project Proposal PDF is in the file named '`Project Design Proposal.pdf`' or a similar name, in the root directory.

Our Code Checkpoint README PDF is in the file named '`Code Checkpoint README - Moebius Transformation in ASCII Art.pdf`' or a similar name, in the root directory.


## Authors

- Hongyi Liu (liuhongyi@jhu.edu)
- Christopher Li (cli135@jhu.edu)
