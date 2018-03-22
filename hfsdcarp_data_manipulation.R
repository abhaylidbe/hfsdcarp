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
