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