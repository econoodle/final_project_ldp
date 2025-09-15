install.packages('palmerpenguins')
library(palmerpenguins)
# load raw data in
data(penguins)

# save data to raw data folder
write.csv(penguins, '00_rawdata/palmerpenguins.csv')
