# --- Day 11: Seating System ---
#   Your plane lands with plenty of time to spare. The final leg of your journey is a ferry that goes directly to the tropical island where you can finally start your vacation. As you reach the waiting area to board the ferry, you realize you're so early, nobody else has even arrived yet!
# 
# By modeling the process people use to choose (or abandon) their seat in the waiting area, you're pretty sure you can predict the best place to sit. You make a quick map of the seat layout (your puzzle input).
# 
# The seat layout fits neatly on a grid. Each position is either floor (.), an empty seat (L), or an occupied seat (#). For example, the initial seat layout might look like this:
#   
"L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL"

#   Now, you just need to model the people who will be arriving shortly. Fortunately, people are entirely predictable and always follow a simple set of rules. All decisions are based on the number of occupied seats adjacent to a given seat (one of the eight positions immediately up, down, left, right, or diagonal from the seat). The following rules are applied to every seat simultaneously:
#     
# - If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
# - If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
# - Otherwise, the seat's state does not change.
# - Floor (.) never changes; seats don't move, and nobody sits on the floor.
#   After one round of these rules, every seat in the example layout becomes occupied:
#       
#         #.##.##.##
#         #######.##
#         #.#.#..#..
#         ####.##.##
#         #.##.##.##
#         #.#####.##
#         ..#.#.....
#         #########
#         .######.#
#         .#####.##
#     After a second round, the seats with four or more occupied adjacent seats become empty again:
#       
#     #.LL.L#.##
#     #LLLLLL.L#
#     L.L.L..L..
#     #LLL.LL.L#
#     #.LL.LL.LL
#     #.LLLL#.##
#     ..L.L.....
#     #LLLLLLLL#
#     #.LLLLLL.L
#     #.#LLLL.##
#     This process continues for three more rounds:
#       
#     #.##.L#.##
#     #L###LL.L#
#     L.#.#..#..
#     #L##.##.L#
#     #.##.LL.LL
#     #.###L#.##
#     ..#.#.....
#     #L######L#
#     #.LL###L.L
#     #.#L###.##

#     #.#L.L#.##
#     #LLL#LL.L#
#     L.L.L..#..
#     #LLL.##.L#
#     #.LL.LL.LL
#     #.LL#L#.##
#     ..L.L.....
#     #L#LLLL#L#
#     #.LLLLLL.L
#     #.#L#L#.##

#     #.#L.L#.##
#     #LLL#LL.L#
#     L.#.L..#..
#     #L##.##.L#
#     #.#L.LL.LL
#     #.#L#L#.##
#     ..L.L.....
#     #L#L##L#L#
#     #.LLLLLL.L
#     #.#L#L#.##

#     At this point, something interesting happens: the chaos stabilizes and further applications of these rules cause no seats to change state! Once people stop moving around, you count 37 occupied seats.
#     
#     Simulate your seating area by applying the seating rules repeatedly until no seats change state. How many seats end up occupied?
#       
#       
library(tidyverse)
rm(list = ls())

#
update_seating <- function(floorplan) {
  # updates the next stage of the seating 
  new <- floorplan
  for (i in seq_along(floorplan)){
    offset <- ncol(floorplan)
    # . means no chair
    if (floorplan[i] == '.') next
    # find neighboring seat occupancy
    if (i %% offset == 1) neighbors <- c(i, i+1)
    else if (i %% offset == 0) neighbors <- c(i-1, i)
    else neighbors <- c(i-1, i, i+1)
    neighbors <- c(neighbors-offset, neighbors, neighbors+offset)
    neighbors <- neighbors[which(neighbors > 0)]
    neighbors <- neighbors[which(neighbors <= length(floorplan))]
    neighbors <- neighbors[which(neighbors != i)]
    occupied <- sum(floorplan[neighbors]=='#')
    # print(neighbors)
    # set next state based on occupancy
    if (occupied >= 4) new[i] <- 'L'
    else if (occupied == 0) new[i] <-  '#'
    else new[i] <- floorplan[i]
  }
  return(new)  
}
#

floorplan <- read_lines('input11.txt')
row_width <- nchar(floorplan[1])
floorplan <- lapply(floorplan, strsplit, '')
floorplan <- matrix(unlist(floorplan), ncol = row_width, byrow = T)
i <- 0
while(T) {
  new_floorplan <- update_seating(floorplan)
  if (all(new_floorplan == floorplan)) {
    break
  } else {
  floorplan <- new_floorplan
  i <- i+1
  }
}
new_floorplan <- update_seating(floorplan)
floorplan <- new_floorplan
nseats <- sum(floorplan == '#')
nseats
