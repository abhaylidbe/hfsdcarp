getwd()
setwd('D:/Google Drive/Research Papers/599 HFSDCARP Project/R script for data manipulation')
getwd()

library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

# source("D:/Google Drive/Research Papers/599 HFSDCARP Project/R script for data manipulation/hfsdcarp_data_manipulation.r")
file_to_read = "D:/Google Drive/Research Papers/599 HFSDCARP Project/R script for data manipulation/hfsdcarp_data_manipulation.r"

# FUCNTION TO READ OTHER R SCRIPTS
sourcePartial <- function(fn, skip=0, n=-1) {
                                      lines <- scan(fn, what=character(), sep="\n", skip=skip, n=n, quiet=TRUE)
                                      tc <- textConnection(lines)
                                      source(tc)
                                      close(tc)
                                    }

# READ THE HFSDCARP DATA MANIPULATION SCRIPT TO CREATE DATA OBJECTS
sourcePartial(file_to_read, 1, 117)


from <- unique(complete_data$from)
to <- unique(complete_data$to)
cost <- as.matrix(spread(subset(complete_data,select=c(from,to,cost)),to,cost)[,-1])
demand <- as.matrix(spread(subset(complete_data,select=c(from,to,demand)),to,demand)[,-1])
veh <- unique(vehicle$no)
capacity <- as.matrix(subset(vehicle))


model <- MIPModel() %>% 

  add_variable(x[i,j,v], i = from, j = to, v = veh, type = "binary") %>% 

  add_variable(y[i,j,v], i = from, j = to, v = veh, type = "binary") %>% 
  
  set_objective(sum_expr(cost[i,j] * x[i,j,v], i = from, j = to, v = veh), "min") %>% 
  
  add_constraint(sum_expr(x[i,j,v] - x[j,i,v], j = to) == 0, i = from, v = veh) %>% 
  
  add_constraint(sum_expr(x[i,j,v] - y[i,j,v]) >= 0, i = from, j = to, v = veh) %>% 
  
  add_constraint(sum_expr(y[i,j,v] + y[j,i,v], i = from, j = to) >= 0, v = veh) %>% 
  
  add_constraint(sum_expr(y[i,j,v] * demand[i,j], i = from, j = to) <= capacity, v = veh) %>% 
  
  solve_model(with_ROI("glpk", verbose = TRUE))

get_solution(model, x[i,j,v]) %>% 
  dplyr::filter(value == 1)

n <- 10; W <- 2
v <- runif(n);
w <- runif(n)
model <- MIPModel() %>% 
  # Variable types
  add_variable(x, type = "integer") %>% 
  add_variable(y, type = "continuous") %>% 
  add_variable(z, type = "binary")
add_variable(x[i], i = 1:n, type = "binary") %>% 
  # Variable bounds
  add_variable(x, lb = 10) %>% 
  add_variable(y, lb = 5, ub = 10)
# Indexed variables
add_variable(x[i], i = 1:10) %>%  # creates 10 decision variables
  # Summation over variables
  add_variable(x[i], i = 1:3) %>% 
  set_objective(sum_expr(x[i], i = 1:3)) %>% 
  add_constraint(sum_expr(x[i], i = 1:3) <= 10)
# Quantifiers
# Create x_{i, j} variables for all combinations of i and j where
# i = 1:10 and j = 1:10.
add_variable(x[i, j], type = "binary", i = 1:10, j = 1:10) %>% 
  # add a y_i variable for all i between 1 and 10 with i mod 2 = 0
  add_variable(y[i], type = "binary", i = 1:10, i %% 2 == 0) %>% 
  # we maximize all x_{i,j} where i = j + 1
  set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10, i == j + 1)) %>% 
  # for each i between 1 and 10 with i mod 2 = 0
  # we add a constraint \sum_j x_{i,j}
  add_constraint(sum_expr(x[i, j], j = 1:10) <= 1, i = 1:10, i %% 2 == 0) %>% 
  # of course you can leave out filters or add more than 1
  add_constraint(sum_expr(x[i, j], j = 1:10) <= 2, i = 1:10) 
# Special bounds on a subset of variables
add_variable(x[i, j], i = 1:10, j = 1:10, type = "integer", lb = 0, ub = 1) %>% 
  set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10)) %>% 
  add_constraint(x[i, i] == 0, i = 1:10) %>% 
  # this sets the ub to 0 without adding new constraints
  set_bounds(x[i, i], ub = 0, i = 1:10)
# External model parameters
n <- 5 # number of our variables
costs <- rpois(n, lambda = 3) # a cost vector
max_elements <- 3
MIPModel() %>% 
  add_variable(x[i], type = "binary", i = 1:n) %>% 
  set_objective(sum_expr(costs[i] * x[i], i = 1:n)) %>% 
  add_constraint(sum_expr(x[i], i = 1:n) <= max_elements)
# Extract model solutions
result <- solve_model(with_ROI("glpk", verbose = TRUE))
get_solution(result, x[i, j]) %>% 
  dplyr::filter(value == 1)
# You can also fix certain indexes.
get_solution(result, x[2, j])
set_objective(sum_expr(v[i] * x[i], i = 1:n)) %>% 
  add_constraint(sum_expr(w[i] * x[i], i = 1:n) <= W)
#=================================================================================
# Knapsack
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
max_capacity <- 5
n <- 10
weights <- runif(n, max = max_capacity)
MIPModel() %>%
  add_variable(x[i], i = 1:n, type = "binary") %>%
  set_objective(sum_expr(weights[i] * x[i], i = 1:n), "max") %>%
  add_constraint(sum_expr(weights[i] * x[i], i = 1:n) <= max_capacity) %>%
  solve_model(with_ROI(solver = "glpk")) %>% 
  get_solution(x[i]) %>% 
  filter(value > 0)
# Bin Packing
# An example of a more difficult model solved by symphony.

library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)
max_bins <- 10
bin_size <- 3
n <- 10
weights <- runif(n, max = bin_size)
MIPModel() %>%
  add_variable(y[i], i = 1:max_bins, type = "binary") %>%
  add_variable(x[i, j], i = 1:max_bins, j = 1:n, type = "binary") %>%
  set_objective(sum_expr(y[i], i = 1:max_bins), "min") %>%
  add_constraint(sum_expr(weights[j] * x[i, j], j = 1:n) <= y[i] * bin_size, i = 1:max_bins) %>%
  add_constraint(sum_expr(x[i, j], i = 1:max_bins) == 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1)) %>% 
  get_solution(x[i, j]) %>%
  filter(value > 0) %>%
  arrange(i)
#=================================================================================
name2 = 'hfsdcarp_vehicle_routing.txt'
result_name = file.path("D:/Google Drive/Research Papers/599 HFSDCARP Project/GAMS",name2)
result_name

open_file2 = read.delim(result_name, header = TRUE, sep = "\t")
open_file2

g2.mat = as.matrix(open_file2[,2:3])
g2 <- graph.edgelist(g2.mat, directed = TRUE)
g2 = set_edge_attr(g2, "vehicle", value = open_file2[,1])


E(g2)$color <- ifelse(E(g2)$vehicle == 5, "red", "blue")
E(g2)$width <- ifelse(E(g2)$vehicle == 5, 7 ,0)
plot(g2, edge.arrow.size = 0.1, layout = layout.kamada.kawai(g2))

#=================================================================================
#plot(g, vertex.label.color = "black", vertex.size = 10, vertex.label.cex	= 1, layout = layout_nicely(g))
plot(g, vertex.label.color = "black", vertex.size = 10, vertex.label.cex	= 1, layout = layout.kamada.kawai(g))


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX





#Copying the data frame to csv file
#
print_data = data.frame("arc" = paste(egl_e1_A$from, egl_e1_A$to, sep=".")
                        , "cost" = egl_e1_A$cost
                        , "demand" = egl_e1_A$demand
)
out_file = ("D:/Google Drive/Research Papers/599 HFSDCARP Project/R script for data manipulation/egl_e1_A.csv")

write.csv(print_data, file = out_file, row.names = FALSE)
