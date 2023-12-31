Final Project - FPSE - Moebius Transformation in ASCII Art
--------------------------------

![Cool animation of the program](images/cool.gif)

![Moebius Transformation Image Screenshot](images/moebius-presentation-screenshot.png)

Functional Programming in Software Engineering

601.429/629   Fall 2023 Final Project

Authors:
- Hongyi Liu,
- Christopher Li

## Overview:

Here's a visualization of the screen you will see upon loading the program:

![Moebius Transformation Main Menu Screenshot](images/ascii-main-menu.png)

- The purpose of this project is to create an animation of the Moebius transformation via a rasterizer.
  - Users can interact with the animation by setting parameters, and the animation is displayed via ASCII art.
  - The goal is to provide an intuitive and interactive visualization of the Moebius transformation and how it is related to the stereographic projection in the command line terminal via ASCII art.

Here is some background information on the Moebius transformation:

![Background info](images/background-slide.png)

![Injective transformation](images/injective.png)

Here are some additional images (an ASCII screenshot and a PNG screenshot):

![Additional Images](images/additional-images.png)

## **How to run the program / Usage:**

- **`opam install . --deps-only --working-dir`** to install dependencies if not already installed
- **`dune b`**
- **`./_build/default/src/user_interface.exe`**
  - This last command runs the program.
  - You will then see an image displayed, with the option to enter commands interactively. Below is a list of commands that are accepted syntax.

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

Note:
- Please use the VSCode terminal on a low `img_w` (i.e. `set img_w 50`) to avoid flickering issues when running the animations on a laptop or similar device. Computers with more resources can run on larger `img_w` because they can redraw the ASCII characters fast enough to avoid any flickering and delay, but on computers with less resources the program works best with small `img_w` and lower `frame_rate` options.
- We configured our program and visualization based on the output of the VSCode terminal, so our group believes the VSCode terminal will provide the best results when running our program. While other terminals may work, we cannot assure their visualization effectiveness.

------------------

To run the `print_ascii.exe` program, which is an image-converter between the rasterizer, general PNG images, and general ASCII images, you can run the following commands:
- **`dune b`**
- **`./_build/default/src/print_ascii.exe`**
  - This last command runs the program.
  - You can then enter the name of a filepath/filename, such as `images/von-neumann.png`, and the PNG will be translated into ASCII.

## Test Coverage

- `math.ml` and `rasterizer.ml`: 92%
- `animation.ml` core logic is covered,
  - test for hardcoded animation is not relevant for / related to testing since it is a visual test
- `ascii_printer.ml`: 90%
- `user_interface.ml`: This is I/O so it is not tested for coverage.


## Update notes:

- Hongyi
  - refactored the user interface extensively,
  - wrote the rasterizer and math libraries, and added the capability for supersampling in the rasterizer, and
  - wrote animations for the parameters and refactored the way the keyframe animations are done (by jointly interpolating parameters).
    - If you type 'cool' in the ./_build/default/src/user_interface.exe application, then a predefined animation will run.
  The updates to the code are in rasterizer.ml, math.ml, and user_interface.ml

- Christopher
  - wrote and refactored the user interface options, including the pattern-matching to parse the command syntax
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

This `Readme.md` document is the Readme document for the Final Project Deliverable.

In the `documents/` directory you can find:
- Our Project Proposal PDF is in the file named '`Project Design Proposal.pdf`' or a similar name.
- Our Code Checkpoint README PDF is in the file named '`Code Checkpoint README - Moebius Transformation in ASCII Art.pdf`' or a similar name.

## Dependencies/Libraries used:

- `Core`
- `OUnit2`
- `Imagelib`
  - This library has been tested and is working now, with the functionality displayed below! Namely, we can read from PNG files, displaying them as ASCII art, and we can write to PNG files, displaying them the rasterizer output as normal PNG images (in print_ascii.ml)
  - The results of reading from PNG files and rendering them as ASCII art is also shown below, which is also included in the title image:
  - ![Moebius Transformation Image Screenshot](images/moebius-presentation-screenshot.png)
- `Yojson`

## Authors

- Hongyi Liu (liuhongyi@jhu.edu)
- Christopher Li (cli135@jhu.edu)
