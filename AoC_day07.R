# --- Day 7: Handy Haversacks ---
#   You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.
# 
# Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!
# 
# For example, consider the following rules:
# 
# light red bags contain 1 bright white bag, 2 muted yellow bags.
# dark orange bags contain 3 bright white bags, 4 muted yellow bags.
# bright white bags contain 1 shiny gold bag.
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
# shiny gold bags contain 1 dark olive bag, 2 vibrant vibrant-plum bags.
# dark olive bags contain 3 faded blue bags, 4 dotted spotted-black bags.
# vibrant vibrant-plum bags contain 5 faded blue bags, 6 dotted spotted-black bags.
# faded blue bags contain no other bags.
# dotted spotted-black bags contain no other bags.

# These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant vibrant-plum bag contains 11 bags (5 faded blue and 6 dotted spotted-black), and so on.
# 
# You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
# 
# In the above rules, the following options would be available to you:
# 
# A bright white bag, which can hold your shiny gold bag directly.
# A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
# A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
# A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
# So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.
# 
# How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)
# 

# digraph edgelist for bags; edge weights correspond to bags that can be carried by node bag
library(tidyverse)

parse_rules <- function(input_file) {
  # makes the edgelist for bags' digraph
  tibble(raw =  read_lines(input_file))  %>%
    separate(raw, sep = ' contain ', into = c('v', 'u')) %>%
    mutate(
      v = str_remove(v, ' bags'),
      u = na_if(u, 'no other bags.'),
      u = str_split(u, ', ')) %>%
    unnest(cols = 'u') %>% 
    mutate(
      weight = parse_number(u),
      u = str_remove_all(u, ' bags.| bag.| bag|^..')
    ) 
}
#
depthfirstsearch <- function(node, edgelist) {
  # add current node to list of visited nodes
  .GlobalEnv$visited <- c(.GlobalEnv$visited, node)

  # grab edges from node 
  u <- edgelist %>%
    filter(v == node & !is.na(u)) %>%
    pull(.data$u)
  # cat(c('\nDestinations: ', paste(u), '\n'))
  
  # break if already found target 'shiny gold' node in dfs from start
  if ('shiny gold' %in% u) {
    .GlobalEnv$found_gold <- T
    .GlobalEnv$has_gold[[node]] <- TRUE
    return()
  }
  # otherwise, continue dfs for all children
  else {
    for (x in u) {
      # break if child node already known to reach target node
      if (.GlobalEnv$has_gold[[x]]) {
        .GlobalEnv$found_gold <- T
        .GlobalEnv$has_gold[[node]] <- TRUE
        return()
      }
      # search child node if not already visited
      if (!x %in% .GlobalEnv$visited) depthfirstsearch(x, edgelist)
      # mark all prior nodes in path as reaching target
      if (.GlobalEnv$found_gold) {
        .GlobalEnv$has_gold[[node]] <- TRUE
        return()
      }
    }
  }
  return()
}

start_dfs <- function(start, edgelist) {
  # flag for finding shiny gold bag from start node
  .GlobalEnv$found_gold <- F
  # init dfs from start node
  if (!has_gold[[start]]) depthfirstsearch(start, edgelist)
  return(.GlobalEnv$found_gold)
  
}

rules <- parse_rules('input7.txt')
baglist <- unique(rules$v)
visited <- c()
has_gold <- rep(F, length(baglist))
names(has_gold) <- baglist

sapply(baglist, function(x) start_dfs(x, rules))
sum(has_gold)


## Part 2 ----

# It's getting pretty expensive to fly these days - not because of ticket prices, but because of the ridiculous number of bags you need to buy!
# 
# Consider again your shiny gold bag and the rules from the above example:
# 
# faded blue bags contain 0 other bags.
# dotted black bags contain 0 other bags.
# vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
# dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
# So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
# 
# Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!
# 
# Here's another example:
#   
#   shiny gold bags contain 2 dark red bags.
# dark red bags contain 2 dark orange bags.
# dark orange bags contain 2 dark yellow bags.
# dark yellow bags contain 2 dark green bags.
# dark green bags contain 2 dark blue bags.
# dark blue bags contain 2 dark violet bags.
# dark violet bags contain no other bags.
# In this example, a single shiny gold bag must contain 126 other bags.
# 
# How many individual bags are required inside your single shiny gold bag?
#   
#   
parse_rules <- function(input_file) {
  # makes the edgelist for bags' digraph
  tibble(raw =  read_lines(input_file))  %>%
    separate(raw, sep = ' contain ', into = c('v', 'u')) %>%
    mutate(
      v = str_remove(v, ' bags'),
      u = na_if(u, 'no other bags.'),
      u = str_split(u, ', ')) %>%
    unnest(cols = 'u') %>% 
    mutate(
      weight = parse_number(u),
      u = str_remove_all(u, ' bags.| bag.| bag|^..')
    ) 
}

how_many_bags <- function(node, edgelist) {
  # reset number of bags in bag to zero
  bags <- 0
  
  # find bag types inside and weights
  children <- edgelist %>%
    filter(v == node & !is.na(u)) %>%
    select(u, weight)
  if (nrow(children) == 0) return(0)

  # search within child bags and tally recursively through dfs
  for (i in 1:nrow(children)){
    child <- children[i, 'u'][[1]]
    # number of current bag inside parent
    weight <- children[i, 'weight'][[1]]
    # number of bags inside each child of this type
    child_bags <- how_many_bags(child, edgelist)
    # cat('\n', child_bags, ' Bags within a child ', child, ' bag')
    # update number of bags
    bags <- bags + weight + (weight * child_bags)
  }
  cat('\n', bags, ' Bags within a ', node, ' bag')
  return(bags)
}

##
edgelist <- parse_rules('input7.txt')
bags <- how_many_bags('shiny gold', edgelist)
bags
