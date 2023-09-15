## code to prepare `mrp_ontology` df
## DO NOT RUN
## only for reproducibility purposes: edit file at inst/mrp_ontology.csv by hand
#
# library(mregions2)
#
# # Define data frame and get all attributes
# df = data.frame(layer = NULL, column = NULL, type = NULL)
#
# for(layer in mrp_list$layer){
#   c = mrp_colnames(layer)
#   df <- rbind(df, c)
# }
#
# # Add order flag
# df$n <- 1:nrow(df)
#
# # Inspect
# View(df)
#
# write.csv2(df, "inst/mrp_ontology.csv", row.names = FALSE,
#  fileEncoding = "UTF-8", quote = FALSE)

path <- system.file("mrp_ontology.csv", package = "mregions2")
mrp_ontology <- read.csv2(path)

# Drop order flag - internal use only
mrp_ontology$n <- NULL

# Turn into tibble
attr(mrp_ontology, "class") <- c("tbl_df", "tbl", "data.frame")

usethis::use_data(mrp_ontology, overwrite = TRUE, ascii = TRUE)




