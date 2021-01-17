# --- Day 12: Rain Risk ---
#   Your ferry made decent progress toward the island, but the storm came in faster than anyone expected. The ferry needs to take evasive actions!

# Unfortunately, the ship's navigation computer seems to be malfunctioning; rather than giving a route directly to safety, it produced extremely circuitous instructions. When the captain uses the PA system to ask if anyone can help, you quickly volunteer.

# The navigation instructions (your puzzle input) consists of a sequence of single-character actions paired with integer input values. After staring at them for a few minutes, you work out what they probably mean:
#
# Action N means to move north by the given value.
# Action S means to move south by the given value.
# Action E means to move east by the given value.
# Action W means to move west by the given value.
# Action L means to turn left the given number of degrees.
# Action R means to turn right the given number of degrees.
# Action F means to move forward by the given value in the direction the ship is currently facing.
# The ship starts by facing east. Only the L and R actions change the direction the ship is facing. (That is, if the ship is facing east and the next instruction is N10, the ship would move north 10 units, but would still move east if the following action were F.)
#
# For example:
#
# F10
# N3
# F7
# R90
# F11
# These instructions would be handled as follows:
#
# F10 would move the ship 10 units east (because the ship starts by facing east) to east 10, north 0.
# N3 would move the ship 3 units north to east 10, north 3.
# F7 would move the ship another 7 units east (because the ship is still facing east) to east 17, north 3.
# R90 would cause the ship to turn right by 90 degrees and face south; it remains at east 17, north 3.
# F11 would move the ship 11 units south to east 17, south 8.
# At the end of these instructions, the ship's Manhattan distance (sum of the absolute values of its east/west position and its north/south position) from its starting position is 17 + 8 = 25.
#
# Figure out where the navigation instructions lead. What is the Manhattan distance between that location and the ship's starting position?
#
library(tidyverse)
dir <- readLines('input12.txt')
dirs <- data.frame(direction = substring(dir, 1, 1),
                   distance = as.numeric(gsub(
                     dir, pattern = '^[A-Z]', replacement = ''
                   )))

part1 <- function(dirs){
  pos <- c(0, 0)
  facing <- 90
  for (i in seq_along(dir)) {
    action <- dirs[i, 1]
    dist <- dirs[i, 2]
    # print(paste(action, dist))
    # print(facing)
    # print(pos)
    if (action %in% c('N', 'S')) {
      pos[2] <- ifelse(action == 'N', pos[2] + dist, pos[2] - dist)
    } else if (action %in% c('E', 'W')) {
      pos[1] <- ifelse(action == 'E', pos[1] + dist, pos[1] - dist)
    } else if (action == 'F') {
      if (facing %in% c(90, 270)) {
        pos[1] <- ifelse(facing == 90, pos[1] + dist, pos[1] - dist)
      } else if (facing %in% c(0, 180)) {
        pos[2] <- ifelse(facing == 0, pos[2] + dist, pos[2] - dist)
      }
    } else if (action %in% c('L', 'R')) {
      facing <- ifelse(action == 'L', facing - dist, facing + dist) %% 360
      # print(facing)
    }
    # print(pos)
    # cat('\n')
  }
  
  abspos <- abs(pos)
  sum(abspos)
}
part1(dirs)
#
# --- Part Two ---
#   Before you can give the destination to the captain, you realize that the actual action meanings were printed on the back of the instructions the whole time.
#
# Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:
#
# Action N means to move the waypoint north by the given value.
# Action S means to move the waypoint south by the given value.
# Action E means to move the waypoint east by the given value.
# Action W means to move the waypoint west by the given value.
# Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
# Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
# Action F means to move forward to the waypoint a number of times equal to the given value.
# The waypoint starts 10 units east and 1 unit north relative to the ship. The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.
#
# For example, using the same instructions as above:
#
# F10 moves the ship to the waypoint 10 times (a total of 100 units east and 10 units north), leaving the ship at east 100, north 10. The waypoint stays 10 units east and 1 unit north of the ship.
# N3 moves the waypoint 3 units north to 10 units east and 4 units north of the ship. The ship remains at east 100, north 10.
# F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28 units north), leaving the ship at east 170, north 38. The waypoint stays 10 units east and 4 units north of the ship.
# R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to 4 units east and 10 units south of the ship. The ship remains at east 170, north 38.
# F11 moves the ship to the waypoint 11 times (a total of 44 units east and 110 units south), leaving the ship at east 214, south 72. The waypoint stays 4 units east and 10 units south of the ship.
# After these operations, the ship's Manhattan distance from its starting position is 214 + 72 = 286.
#
# Figure out where the navigation instructions actually lead. What is the Manhattan distance between that location and the ship's starting position?

# Proceed to waypoint 'F' function
Forward <- function(waypoint, position, distance){
  # F <- move ship to waypoint x times, return position
  return(position + distance * waypoint)
}

# Rotations: 'L', 'R' functions
r90 <- function(waypoint){ # rotate cw 90
  return(c(waypoint[2], -waypoint[1]))
}
l90 <- function(waypoint){ # rotate ccw 90
  return(c(-waypoint[2], waypoint[1]))
}
LR <- function(waypoint, dir, dist){
  # L, R <- rotate waypoint relative to ship
  if (dist == 90) { # perform rotation 90 on waypt
    if (dir == 'R') return(r90(waypoint))
    else if (dir == 'L') return(l90(waypoint))
  }
  else if (dist == 270){ # if 270, do 90 in opposite direction
    if (dir=='L') return(r90(waypoint))
    else if (dir=='R') return(l90(waypoint))
  }
  # if 180, take negative of vector
  else if (dist == 180) return(-1 * waypoint)
}

NSEW <- function(waypoint, dir, dist) {
  # NSEW <- moves waypoint x distance in direction
  if (dir %in% c('E', 'W')) {
    waypoint[1] <-
      ifelse(dir == 'E', waypoint[1] + dist, waypoint[1] - dist)
  } else if (dir %in% c('N', 'S')) {
    waypoint[2] <-
      ifelse(dir == 'N', waypoint[2] + dist, waypoint[2] - dist)
  }
  return(waypoint)
}

move_ship <- function(instructions){
  
  # use (x,y) coordinates [(+E,-W), (+N,-S)]
  waypoint <- c(10, 1)
  position <- c(0, 0)
  
  for (i in seq_along(dirs$direction)){
    action <- instructions[i, 1]
    dist <- instructions[i, 2]
    if (action == 'F') {
      position <- Forward(waypoint, position, dist)
    } else if (action %in% c('L','R')) {
      waypoint <- LR(waypoint, action, dist)
    } else if (action %in% c('E', 'W', 'N', 'S')) {
      waypoint <- NSEW(waypoint, action, dist)
    }
  }
  return(position)
}
###

dir <- readLines('input12.txt')
# split action + distance from instructions
instructions <- data.frame(
  direction = substring(dir, 1, 1),
  distance = as.numeric(gsub(dir, pattern = '^[A-Z]', replacement = ''))
  )
# move ship based on instructions
position <- move_ship(instructions)
manhattan_dist <- sum(abs(position))
'solution 2'
manhattan_dist
