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
