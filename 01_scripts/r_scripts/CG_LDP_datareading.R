install.packages('palmerpenguins')
library(palmerpenguins)
# load raw data in
data(penguins)

# save data to raw data folder
write.csv(penguins, '00_rawdata/CG_LDP_penguins_1.csv', row.names = F)
renv::init()
renv::status()
renv::snapshot()
