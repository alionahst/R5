    # Chapter 5: Hypothesis testing - Aliona Hoste 
# Create a rmarkdown document with the answer to the following questions. 
# Change knitr options to save all the graphics in the folder “results” in png format with a resolution of 100 dpi.
# Note: to change the knitr options use the command knitr::opts_chunk$set().




# Exercice 1: For the data InsectSpray, make a table for the number of insects for each spray with the mean, median and standard error.
    # Note: Remember to use knitr::kable(), or a similar function to print the table with its caption.

InsectSprays <- InsectSprays # to visualise the data frame 

# create empty vectors so that we can use it in for loops
Spray <- factor()
Count <- integer()
SS <- integer()
Mean <- integer()
Median <- integer()
Standard_error <- integer()

# create Spray vectors with all the levels 
for(i in levels(InsectSprays$spray)){
  Spray <- c(Spray,i)
}
# or Spray <- levels(InsectSprays$spray)

# create the Count vector where i takes each value of Spray (A, B, C...) and do the sum of count for each Spray categories
for(i in Spray){
  SS <- subset(InsectSprays$count,InsectSprays$spray == i)
  Count <- c(Count, sum(SS))
  Mean <- c(Mean, mean(SS))
  Median <- c(Median, median(SS))
  Standard_error <- c(Standard_error, sd(SS)/sqrt(length(SS)))
}

# Create the data frame
Datatable <- data.frame(Spray, Count, Mean, Median, Standard_error)





# Exercice 2: Print a plot to see the differences of counts between sprays. 
# Include a caption explaining the figure. 
# Which type of plot is the one you choose and why? barplot because we have a continuous variable in function of categories
barplot(Datatable$Count ~ Datatable$Spray, Datatable, col= as.factor(Datatable$Spray), ylab = "Count", xlab = "Spray") # plot Count for each spray 

title(main="Number of insects for each spray") # add a title
legend("top", legend = paste("Spray =", Spray), lty = 1, col = 1:6) # add the legend



# Exercice 3:Test for differences between sprays using anova and a posthoc comparison and redo the previous plot including the representation of all posthoc differences.
    # Note: for the anova use the command aov() 
    # and for the posthoc comparison use the Tukey’s ‘Honest Significant Difference’ method. For this method try the TukeyHSD() and the agricolae::HSD.test() and see the differences.


bla <- aov(Count ~ Spray, data = Datatable)

TukeyHSD(bla)


# Exercice 4: Test for differences between sprays using non-parametric Kruskal-Wallis rank sum test. Again, redo the plot with these results.
    # Note: Use agricolae::kruskal().



# Exercice 5: Transform count data using sqrt(counts) and redo the anova, the Tukey posthoc comparison and the plot.




# Exercice 6: Test for normality of residuals for the two performed anova analyses of points 4 and 6 using shapiro.test() and use plot the anova to see the qqplots and compare them.




# Exercice 7:Which of the previous analysis is the adequate in this case? Why? 
# Is there any difference in the results between the square root transformed ANOVA and the Kruskal-Wallis analyses? 
# Is there any difference in the results between the direct ANOVA and the square root transformed ANOVA? Which ones?






