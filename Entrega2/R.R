# i.
preus_nodals_mat <- matrix(c(30, 24.4814, 69.6, 30, 30,
                             32.5186, 24.4814, 90, 60.727, 57.7047,
                             35, 24.4814, 78, 91, 85,
                             33.5112, 24.4814, 85.2, 72.8362, 68.6228),
                           nrow = 4, byrow = TRUE)

preus_nodals_df <- as.data.frame(preus_nodals_mat)

rownames(preus_nodals_df) <- 1:4
colnames(preus_nodals_df) <- paste("Temps", 1:5, sep = " ")

print(preus_nodals_df)