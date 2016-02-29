# dump tidy data produced using run_analysis.R into txt file
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)