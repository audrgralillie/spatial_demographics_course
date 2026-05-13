# Set working directory to the file with data in it
setwd("~/Desktop/spatialdemograph/DSS")

# Read CSV and save its contents as object 'star'
star <- read.csv("STAR.csv")


#View dataset object
view(star)
#View only the first six lines
head(star) # use head(star, n = 3) to see the first three rows
# classtype reading math graduated
# 1     small     578  610         1
# 2   regular     612  612         1
# 3   regular     583  606         1

# Identify the number of observations (dimensions) as rows, columns
dim(star)
# [1] 1274  4
# and now we know n = 1274

# Print all of the observations of variable 'reading'
star$reading
# [1] 578 612 583 661 614 610 595 665 616 624 ...
# [26] 565 650 552 636 567 554 595 620 639 633 ...
# ...

# Identify mean of variable 'reading' within 'star'
mean(star$reading) # non-binary -> number
# [1] 628.803      # actual score
mean(star$graduated) # binary -> percent
# [1] 0.8697017.   # percent