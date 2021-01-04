# --- Part One ---
# Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
# Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
# 
# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
# 
# Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
#   
#   

expenses <- read.delim('input1.txt', sep = '\n', header = F)
expenses <- expenses$V1
expenses <- sort(expenses)
solution <- FALSE
for (i in seq_along(expenses)) {
  print(i)
  print(expenses[i])
  if ((2020-expenses[i]) %in% expenses) {
    solution <- (2020 - expenses[i]) * expenses[i]
    print(paste('solved', solution, expenses[i]))
    break
    }
}

# --- Part Two ---
# The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.
# 
# Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.
# 
# In your expense report, what is the product of the three entries that sum to 2020?
#   
#   
solution <- F
for (i in expenses) {
  for (j in rev(expenses)){
    if ((2020-i-j) %in% expenses) {
      print(paste(i, j, (2020-i-j), (2020-i-j)*i*j))
      solution <- (2020-i-j)*i*j
      break
    }  
  if (solution) break
  }
}
  # if ((2020-i) %in% expenses) {
  #   solution <- (2020 - i) * i
  #   print(paste('solved', solution, i))
  #   break
  

