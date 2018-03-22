getwd()
setwd('D:/Google Drive/Research Papers/599 HFSDCARP Project/R script for data manipulation')
getwd()

library(qdap)
library(tm)
library(tidyr)
library(igraph)

# name of the file
name = 'kshs3.txt'

#=================================================================================
#=================================================================================
# INSTRUCTIONS FOR CONVERTING DAT FILES TO TXT FILES                            ||
# 1. select all                                                                 ||
# 2. replace space and space (i.e. two spaces) with single space                ||
# 3. repeat 2. several times to make sure the data set has only single spaces   ||
# 4. select 'Extended' in the search mode                                       ||
# 5. replace space with tab (\t)                                                ||
# 6. save file as txt file                                                      ||
#=================================================================================
#=================================================================================

file_name = file.path("D:/Google Drive/Research Papers/599 HFSDCARP Project/R script for data manipulation/hfsdcarp_data_sets_for_r_plotting_network",name)
file_name

open_file = read.delim(file_name, header = FALSE, sep = "\t")
open_file
class(open_file)
class(open_file[5,3])

#=================================================================================
#=================================================================================
# getting the tag numbers to create subset of data frame
req_arc_from = which(open_file == "LISTA_ARISTAS_REQ") + 1
req_arc_from
last_row = which(open_file == "DEPOSITO")
last_row
#=================================================================================
if (open_file[5,3] != 0) {
      nreq_arc_from = which(open_file == "LISTA_ARISTAS_NOREQ") + 1 
      nreq_arc_to = last_row - 1
      req_arc_to = nreq_arc_from - 2
      nreq_arc_from
      } else {
              req_arc_to = last_row - 1 
              req_arc_to
            }
#=================================================================================
# this is where the single data frame will be splitted into three
df = as.data.frame(open_file)
df
class(df)
#=================================================================================
# the required arcs data frame
r_arcs = df[req_arc_from:req_arc_to,]
r_arcs
reqd_arcs_data_1 = data.frame("from" = r_arcs[,1]
                            , "to" = r_arcs[,2]
                            , "cost" = r_arcs[,4]
                            , "demand" = r_arcs[,6]
                            , "type" = "reqd"
                            )
reqd_arcs_data_2 = data.frame("from" = r_arcs[,2]
                              , "to" = r_arcs[,1]
                              , "cost" = r_arcs[,4]
                              , "demand" = r_arcs[,6]
                              , "type" = "reqd"
                            )
reqd_arcs_data = rbind(reqd_arcs_data_1, reqd_arcs_data_2)
reqd_arcs_data
#=================================================================================
# the non-required arcs data frame
if (open_file[5,3] != 0) {
                          nr_arcs = df[nreq_arc_from:nreq_arc_to,]
                          nr_arcs
                          non_reqd_arcs_data_1 = data.frame("from" = nr_arcs[,1]
                                                          , "to" = nr_arcs[,2]
                                                          , "cost" = nr_arcs[,4]
                                                          , "demand" = nr_arcs[,6]
                                                          , "type" = "non-reqd"
                                                          )
                          non_reqd_arcs_data_2 = data.frame("from" = nr_arcs[,2]
                                                            , "to" = nr_arcs[,1]
                                                            , "cost" = nr_arcs[,4]
                                                            , "demand" = nr_arcs[,6]
                                                            , "type" = "non-reqd"
                                                            )
                          non_reqd_arcs_data = rbind(non_reqd_arcs_data_1, non_reqd_arcs_data_2)
                          non_reqd_arcs_data
                          }
#=================================================================================
# building a single data frame of nodes and edges
if (open_file[5,3] != 0) {
                          complete_data = rbind(reqd_arcs_data, non_reqd_arcs_data)
                          complete_data
                          } else {
                                  complete_data = reqd_arcs_data
                                  complete_data
                                  }
#=================================================================================
# gather general information about the network
complete_data_info = data.frame("vertices" = df[3,3]
                                , "reqd_arcs" = df[4,3]
                                , "non-reqd_arcs" = df[5,3]
                                , "#_vehicles" = df[6,3]
                                , "orig_cap" = df[7,3]
)
complete_data_info
#=================================================================================
# vehicle information
vehicle = data.frame(no = 1:(as_numeric2(complete_data_info$X._vehicles))
                     , capacity = complete_data_info$orig_cap
                    )
vehicle
#=================================================================================

#=================================================================================
# plot the network
g.mat = as.matrix(complete_data[,1:2])
g <- graph.edgelist(g.mat, directed = FALSE)
#=================================================================================
# Subset vertices and edges
V(g)
E(g)
# Count number of edges
gsize(g)
# Count number of vertices
gorder(g)
# set edge attributes
g = set_edge_attr(g, "cost", value = complete_data[,3])
g = set_edge_attr(g, "demand", value = complete_data[,4])
g = set_edge_attr(g, "type", value = complete_data[,5])
# View edge attributes of graph object
edge_attr(g)
names(edge_attr(g))
#=================================================================================
E(g)$color <- ifelse(E(g)$type < 2, "blue", "black")
E(g)$width <- ifelse(E(g)$type < 2, 3 ,1)
plot(g, vertex.label.color = "black", vertex.size = 10, vertex.label.cex	= 1, layout = layout.kamada.kawai(g))
#=================================================================================
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#=================================================================================

install.packages("ompr")
install.packages("ompr.roi")
install.packages("ROI.plugin.cplex")

library(dplyr)
library(ROI)
library(ROI.plugin.cplex)
library(ompr)
library(ompr.roi)

model <- hfsdcarp() %>% 
  add_variable(x[i,j,v], type = "integer", i = complete_data$from, j = complete_data$to, v = vehicle$no, type = "binary") %>% 
  add_variable(y[i,j,v], type = "integer", i = reqd_arcs_data$from, j = reqd_arcs_data$to, v = vehicle$no, type = "binary") %>% 
  set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10, i == j + 1)) %>% 

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
plot(g, vertex.label.color = "black", vertex.size = 10, vertex.label.cex	= 1, layout = layout.fruchterman.reingold(g))


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

