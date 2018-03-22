#Copying the data frame to csv file
#
print_data = data.frame("arc" = paste(egl_e1_A$from, egl_e1_A$to, sep=".")
                        , "cost" = egl_e1_A$cost
                        , "demand" = egl_e1_A$demand
)
out_file = ("D:/Google Drive/Research Papers/599 HFSDCARP Project/R script for data manipulation/egl_e1_A.csv")

write.csv(print_data, file = out_file, row.names = FALSE)