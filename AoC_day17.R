# --- Day 17: Conway Cubes ---

#   As your flight slowly drifts through the sky, the Elves at the Mythical Information Bureau at the North Pole contact you. They'd like some help debugging a malfunctioning experimental energy source aboard one of their super-secret imaging satellites.
# The experimental energy source is based on cutting-edge technology: a set of Conway Cubes contained in a pocket dimension! When you hear it's having problems, you can't help but agree to take a look.
# The pocket dimension contains an infinite 3-dimensional grid. At every integer 3-dimensional coordinate (x,y,z), there exists a single cube which is either active or inactive.
# In the initial state of the pocket dimension, almost all cubes start inactive. The only exception to this is a small flat region of cubes (your puzzle input); the cubes in this region start in the specified active (#) or inactive (.) state.
# The energy source then proceeds to boot up by executing six cycles.
# Each cube only ever considers its neighbors: any of the 26 other cubes where any of their coordinates differ by at most 1. For example, given the cube at x=1,y=2,z=3, its neighbors include the cube at x=2,y=2,z=2, the cube at x=0,y=2,z=3, and so on.

# During a cycle, all cubes simultaneously change their state according to the following rules:

# If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
# If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
# The engineers responsible for this experimental energy source would like you to simulate the pocket dimension and determine what the configuration of cubes should be at the end of the six-cycle boot process.

# For example, consider the following initial state:

# .#.
# ..#
# ###

# Even though the pocket dimension is 3-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1 region of the 3-dimensional space.)
# 
# Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given z coordinate (and the frame of view follows the active cells in each cycle):
# 
# Before any cycles:
# 
# z=0
# .#.
# ..#
# ###
# 
# 
# After 1 cycle:
# 
# z=-1
# #..
# ..#
# .#.
# 
# z=0
# #.#
# .##
# .#.
# 
# z=1
# #..
# ..#
# .#.
# 
# 
# After 2 cycles:
# 
# z=-2
# .....
# .....
# ..#..
# .....
# .....
# 
# z=-1
# ..#..
# .#..#
# ....#
# .#...
# .....
# 
# z=0
# ##...
# ##...
# #....
# ....#
# .###.
# 
# z=1
# ..#..
# .#..#
# ....#
# .#...
# .....
# 
# z=2
# .....
# .....
# ..#..
# .....
# .....
# 
# 
# After 3 cycles:
# 
# z=-2
# .......
# .......
# ..##...
# ..###..
# .......
# .......
# .......
# 
# z=-1
# ..#....
# ...#...
# #......
# .....##
# .#...#.
# ..#.#..
# ...#...
# 
# z=0
# ...#...
# .......
# #......
# .......
# .....##
# .##.#..
# ...#...
# 
# z=1
# ..#....
# ...#...
# #......
# .....##
# .#...#.
# ..#.#..
# ...#...
# 
# z=2
# .......
# .......
# ..##...
# ..###..
# .......
# .......
# .......
# After the full six-cycle boot process completes, 112 cubes are left in the active state.
# 
# Starting with your given initial configuration, simulate six cycles. How many cubes are left in the active state after the sixth cycle?
# 
library(tidyverse)
rm(list = ls())

parse_grid <- function(file){
  grid <- read_lines(file)  %>% 
    str_replace_all('\\.', '0') %>%
    str_replace_all('#', '1') %>%
    sapply(str_split, '') %>% 
    sapply(as.numeric) %>%
    as.array(dim=c(3,3,1)) %>% 
    t()
  rownames(grid) <- NULL
  return(grid)
}

# make_new_grid <- function(grid) {
#   matrix(rep(0, (dim(grid)[1]+2)**2), nrow = dim(grid)[1]+2)
# }
make_new_cube <- function(cube){

  l <- dim(cube)[1] + 2
  if (length(dim(cube))>2) z <- dim(cube)[3] + 2
  else z <- 3
  new_cube <- array(rep(0, l*l*z), dim = c(l,l,z))
  return(new_cube)
}
# remove strata of all zeroes (should only apply to outermost strata!)
remove_empty_strata <- function(cube){
  # cube[ , , !apply(cube, 3, function(x) all(x == 0))]
}
convert_cube <- function(cube){
  new_cube <- make_new_cube(cube)
  
}

###
cube <- parse_grid('test.txt')
new_cube <- make_new_cube(cube)
new_cube

dim(cube)
dim(new_cube)
# neighbors <- function(x,y,z, cube){
#   xrange <- ifelse(x>0, x-1:x+1, 0:2)
#   
#   below <- 
# }

XYZ <- dim(new_cube)
for (z in seq(XYZ[3])) {

  if (length(dim(cube)) == 2){
    prev_levl <- cube
    prev_above <- array(0, dim = dim(cube))
    prev_below <-array(0, dim = dim(cube))
  }
  else{
    prev_levl <- cube[,,z]
    prev_above <- ifelse(z < dim(new_cube)[3], 
                         cube[,,z+1], 
                         array(0, dim = dim(cube)))
    prev_below <-ifelse(z > 1, 
                        cube[,,z-1], 
                        array(0, dim = dim(cube)))
  }
  for (x in seq(XYZ[1])){
    xcenter <- ifelse() 
    xleft <- 
    xright <- 
    for (y in seq(XYZ[2])){
      print(paste(x, y, z))
      new_cube[x,y,z] <- 9
    }
  }
}





