source("R/helpers.R")

item_a <- collect_place_data("A", "histogram", "", c(0, 0, 0, 1, 8))
item_b <- collect_place_data("B", "vector", "c(1,4,1,5,4,5,5,5,4,4)", c(0, 0, 0, 0, 0))

ratings_df <- rbind(item_a, item_b)
ratings_df$group <- factor(ratings_df$group, levels = c("A", "B"))
ratings_df$place <- c(rep("Histogram example", nrow(item_a)), rep("Vector example", nrow(item_b)))

frequentist <- frequentist_analysis(ratings_df)

cat("Item A n =", sum(ratings_df$group == "A"), "\n")
cat("Item B n =", sum(ratings_df$group == "B"), "\n")
cat("Mean(B) - Mean(A):", frequentist$difference_in_means, "\n")
cat("p-value:", frequentist$p_value, "\n")
