# --- Day 16: Ticket Translation ---

#   As you're walking to yet another connecting flight, you realize that one of the legs of your re-routed trip coming up is on a high-speed train. However, the train ticket you were given is in a language you don't understand. You should probably figure out what it says before you get to the train station after the next flight.

# Unfortunately, you can't actually read the words on the ticket. You can, however, read the numbers, and so you figure out the fields these tickets must have and the valid ranges for values in those fields.

# You collect the rules for ticket fields, the numbers on your ticket, and the numbers on other nearby tickets for the same train service (via the airport security cameras) together into a single document you can reference (your puzzle input).

# The rules for ticket fields specify a list of fields that exist somewhere on the ticket and the valid ranges of values for each field. For example, a rule like class: 1-3 or 5-7 means that one of the fields in every ticket is named class and can be any value in the ranges 1-3 or 5-7 (inclusive, such that 3 and 5 are both valid in this field, but 4 is not).

# Each ticket is represented by a single line of comma-separated values. The values are the numbers on the ticket in the order they appear; every ticket has the same format. For example, consider this ticket:

# .--------------------------------------------------------.
# | ????: 101    ?????: 102   ??????????: 103     ???: 104 |
# |                                                        |
# | ??: 301  ??: 302             ???????: 303      ??????? |
# | ??: 401  ??: 402           ???? ????: 403    ????????? |
# '--------------------------------------------------------'

# Here, ? represents text in a language you don't understand. This ticket might be represented as 101,102,103,104,301,302,303,401,402,403; of course, the actual train tickets you're looking at are much more complicated. In any case, you've extracted just the numbers in such a way that the first number is always the same specific field, the second number is always a different specific field, and so on - you just don't know what each position actually means!

# Start by determining which tickets are completely invalid; these are tickets that contain values which aren't valid for any field. Ignore your ticket for now.

# For example, suppose you have the following notes:
"
  class: 1-3 or 5-7
  row: 6-11 or 33-44
  seat: 13-40 or 45-50

  your ticket:
    7,1,14

  nearby tickets:
    7,3,47
    40,4,50
    55,2,20
    38,6,12
"
# It doesn't matter which position corresponds to which field; you can identify invalid nearby tickets by considering only whether tickets contain values that are not valid for any field. In this example, the values on the first nearby ticket are all valid for at least one field. This is not true of the other three nearby tickets: the values 4, 55, and 12 are are not valid for any field. Adding together all of the invalid values produces your ticket scanning error rate: 4 + 55 + 12 = 71.

# Consider the validity of the nearby tickets you scanned. What is your ticket scanning error rate?
# 


parse_rules <- function(input) {
  # to parse the 'x-y or a-b' ranges to sequences
  parse_ranges <- function(text){
    ranges <- unlist(strsplit(text, ' or '))
    ranges <- sapply(ranges, strsplit, '-')
    ranges <- lapply(ranges, as.numeric)
    ranges <- lapply(ranges, function(x) seq(x[1], x[2]))
    return(Reduce(c, ranges))
  }
  
  # get the ranges from rules; type of value doesn't matter here
  # take all lines until first blank; split text to get ranges
  rules <- list()
  for (line in input){
    if (line == '')  break
    else rules[length(rules)+1] <- line
  }
  rules <- sapply(rules, strsplit, ': ')
  rules <- sapply(rules, '[[', 2)
  # get a list of ranges
  ranges <- sapply(rules, parse_ranges)
  # concat all and keep unique values only
  ranges <- unique(unlist(ranges))
  return(ranges)
}

parse_tickets <- function(input) {
  tickets <- list()
  read <- F
  for (line in input) {
    if (read == F) {
      if (line == '')
        read <- TRUE
      next
    }
    else {
      if (line == '' | grepl(':', line))
        next
      else
        tickets[line] <- strsplit(line, ',')
    }
  }
  from_tickets <- unlist(lapply(tickets, as.numeric))
  names(from_tickets) <- NULL
  return(from_tickets)
}

input <- readLines('input16.txt')
valid_vals <- parse_rules(input)
ticket_vals <- parse_tickets(input)

sacnning_error_rate <- sum(ticket_vals[which(!ticket_vals %in% valid_vals)])
sacnning_error_rate

# 
# -- Part Two ---
#   Now that you've identified which tickets contain invalid values, discard those tickets entirely. Use the remaining valid tickets to determine which field is which.
 
# Using the valid ranges for each field, determine what order the fields appear on the tickets. The order is consistent between all tickets: if seat is the third field, it is the third field on every ticket, including your ticket.

# For example, suppose you have the following notes:
"  class: 0-1 or 4-19
  row: 0-5 or 8-19
  seat: 0-13 or 16-19

  your ticket:
  11,12,13

  nearby tickets:
  3,9,18
  15,1,5
  5,14,9"

# Based on the nearby tickets in the above example, the first position must be row, the second position must be class, and the third position must be seat; you can conclude that in your ticket, class is 12, row is 11, and seat is 13.

# Once you work out which field is which, look for the six fields on your ticket that start with the word departure. What do you get if you multiply those six values together?
# 

rm(list=ls())

# 
parse_rules2 <- function(input) {
  parse_ranges <- function(text){
    ranges <- unlist(strsplit(text, ' or '))
    ranges <- sapply(ranges, strsplit, '-')
    ranges <- lapply(ranges, as.numeric)
    ranges <- lapply(ranges, function(x) seq(x[1], x[2]))
    return(Reduce(c, ranges))
  }
  #
  rules <- list()
  for (line in input){
    if (line == '')  break
    else rules[length(rules)+1] <- line
  }
  rules <- sapply(rules, strsplit, ': ')
  fields <- sapply(rules, '[[', 1)
  ranges <- sapply(rules, '[[', 2)
  names(ranges) <- fields
  ranges <- lapply(ranges, parse_ranges)
  return(ranges)
}

parse_myticket <- function(input){
  read <- F
  for (line in input){
    if (read) {
      myticket <- line 
      break
    } else if (grepl('your ticket', line)) {
      read <- T
    }
  }
  myticket <- as.integer(str_split(myticket, ',', simplify = T))
  return(myticket)
}

parse_tickets2 <- function(input) {
  tickets <- list()
  read <- F
  for (line in input) {
    if (read == F) {
      if (line == '')
        read <- TRUE
      next
    }
    else {
      if (line == '' | grepl(':', line))
        next
      else
        tickets[line] <- strsplit(line, ',')
    }
  }
  from_tickets <- lapply(tickets, as.numeric)
  # names(from_tickets) <- NULL
  return(from_tickets)
}

validate_ticket <- function(ticket, allowed_vals){
  if (all(ticket %in% allowed_vals)) TRUE
  else FALSE
}

#   Now that you've identified which tickets contain invalid values, discard those tickets entirely. Use the remaining valid tickets to determine which field is which.

library(tidyverse)
# parse input: get a list of valid numbers and possibly valid tickets
input <- readLines('input16.txt')
myticket <- parse_myticket(input)
rules <- parse_rules2(input)
allowed_vals <- unique(unlist(rules))
tickets <- parse_tickets2(input)
rm(input)

# discard invalid tickets that have any illegal values, not validated
valid <- sapply(tickets, validate_ticket, allowed_vals)
tickets <- tickets[which(valid)]
rm(allowed_vals, valid)

# munge: get values from each field into column vector
tickets <- enframe(tickets) 
tickets <- tickets %>%
  select(-name) %>%
  unnest_wider(col = value)
names(tickets) <- seq_along(tickets)

# 
fits <- matrix(0, nrow = length(rules), ncol = length(tickets))
for (i in seq_along(rules)) {
  for (j in seq_along(tickets)) {
    if (all(tickets[, j][[1]] %in% rules[[i]])) {
      fits[i, j] <- 1
    }
  }
}
rownames(fits) <- names(rules)
colnames(fits) <- names(tickets)
print(data.frame(fits))

# reorder rules from fewest fits to most fits with ticket fields
tickets <- tickets[, c(names(sort(colSums(fits))))]
rules <- rules[names(sort(rowSums(fits)))]
str(rules)
str(tickets)

# Recursive function to assign rules to ticket fields
# if rules is empty, we already found a solution
# else, check ticket fields for matches; 
# if match found, remove rule & field, log the combo in list
# continue recursion on reduced lists until lists are empty (same length)
assign_rules <- function(rules, tickets){
  if (length(rules) == 0) {
    .GlobalEnv$solved = T
    return(':)')
  }
  rule <- rules[1][[1]]
  rule_name <- names(rules[1])
  for (j in seq_along(tickets)){
    if (all(tickets[, j][[1]] %in% rule)){
      .GlobalEnv$solution[names(tickets)[j]] <- rule_name
      assign_rules(rules[-1], tickets[, -j])
    }
    if (.GlobalEnv$solved == T) return(':)')
  }
}
# Assign rules to ticket fields
solved <- F
solution <- list()
# fills in the solution list by assigning rule-ticket field pairs to list
# rules and fields can have multiple compatible matches, but need to search through possibilities to find right way to assign
assign_rules(rules, tickets)



# Once you work out which field is which, look for the six fields on your ticket that start with the word departure. What do you get if you multiply those six values together?

departure_cols <- names(solution)[which(str_detect(solution, 'departure'))]
departure_cols <- as.numeric(departure_cols)
myticket[departure_cols]
as.character(prod(myticket[departure_cols]))
