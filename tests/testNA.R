# Note a 'real' NA and a string with the word "NA"
x <- factor(c("hello", NA, "world", "NA"))

print(x)
# [1] hello <NA>  world NA   
# Levels: hello NA world      <~~ The string appears as a level, the actual NA does not. 

print(as.numeric(x))              
# [1]  1 NA  3  2            <~~ The string has a numeric value (here, 2, alphabetically)
# The NA's numeric value is just NA

