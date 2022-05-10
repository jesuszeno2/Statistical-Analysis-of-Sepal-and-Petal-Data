# Jesus Zeno

# Import the library datasets
library(datasets)

# Use the iris data from that package
data(iris)

# Preview data with just the first several rows
head(iris)

# calculate the area of petal and sepal
iris$petal_area = (iris$Petal.Length * iris$Petal.Width * pi) / 4
iris$sepal_area = (iris$Sepal.Length * iris$Sepal.Width * pi) / 4

# Check results with first few rows
head(iris)

# Get overall summary of dataset
summary(iris)

# determine variance of columns
var(iris$petal_area)
var(iris$sepal_area)

# We can just use var.equal and paired as FALSE. It will be
# the same result as if var.equal happened to be true. Plus, when the 
# variance isn't equal, that means its being more conservative and 
# the assumption is harder to reject. The tests will be paired since
# we have the same individuals in groups and they are in the same order.
# Pairing will make the assertion about the individual as opposed to
# the group. 

# Determine if the sepal area is different than the petal area.
t.test(iris$sepal_area, iris$petal_area, var.equal = FALSE, 
       paired = FALSE, alternative ="two.sided" )
# The t value is 25.45 and that shows how the two groups are different.
# The p-value is < 2.2e-16 which is statistically extremely significant
# (p-value < .001). This highly supports the idea that
# there is a difference between the sepal area and the petal area

# determine if the petal length is different than the petal width
t.test(iris$Petal.Length, iris$Petal.Width, var.equal = FALSE, 
       paired = FALSE, alternative ="two.sided" )
# The t=16.297 which is high enough to show the two groups are different.
# The p-value=2.2e-16 which means it is extremely significant (p-value < .001). 
# This test highly supports the idea that the petal length and width
# are different. 

# Determine if sepal length is different than sepal width.
t.test(iris$Sepal.Length, iris$Sepal.Width, var.equal = FALSE, 
       paired = FALSE, alternative ="two.sided" )
# The t=36.463 which is high enough to show the two groups are different.
# The p-value=2.2e-16 which means it is extremely significant (p-value < .001). 
# This test highly supports the idea that the petal length and width
# are different. 

# Determine if petal length is different than sepal length
t.test(iris$Petal.Length, iris$Sepal.Length, var.equal = FALSE, 
       paired = FALSE, alternative ="two.sided" )
# The t=-13.098 which is negative enough to show the two groups are 
# different. The negative value indicates that the petal lengths are 
# smaller than the sepal lengths. 
# The p-value=2.2e-16 which means it is extremely significant (p-value < .001). 
# This test highly supports the idea that the petal length and width
# are different.

# Determine if petal width is different than sepal width
t.test(iris$Petal.Width, iris$Sepal.Width, var.equal = FALSE, 
       paired = FALSE, alternative ="two.sided" )
# The t=-25.916 which is negative enough to show the two groups are 
# different. The negative value indicates that the petal widths are 
# smaller than the sepal widths. 
# The p-value=2.2e-16 which means it is extremely significant (p-value < .001). 
# This test highly supports the idea that the petal length and width
# are different.

# Conclusion of t-tests: Since all results had p-values <.001 we can reject
# the null hypothesis and understand that there is a significant difference
# between the 6 variables. 


# ANOVA PART FOCUS ON THE GROUPS INSTEAD OF INDIVIDUALS
# Conduct an ANOVA on each of the six measurements you now have, based on the
# type of iris you have. 

# Let's do Petal area first
petal_area_vs_species = aov(iris$petal_area~iris$Species)
summary(petal_area_vs_species)
# Because F value is high (683.1) and that means there 
# is a large difference among the groups and the p value says it's 
# extremely statistically significant <.001

# Let's do Sepal area
sepal_area_vs_species = aov(iris$sepal_area~iris$Species)
summary(sepal_area_vs_species)
# Because F value is high (14.24) and that means there 
# is a large difference among the groups and the p value says it's 
# extremely statistically significant <.001

# Let's do petal length
petal_length_vs_species = aov(iris$Petal.Length~iris$Species)
summary(petal_length_vs_species)
# Because F value is high (1180) and that means there 
# is a large difference among the groups and the p value says it's 
# extremely statistically significant <.001

# Let's do petal width
petal_width_vs_species = aov(iris$Petal.Width~iris$Species)
summary(petal_width_vs_species)
# Because F value is high (960) and that means there 
# is a large difference among the groups and the p value says it's 
# extremely statistically significant <.001

# Let's do sepal length
sepal_length_vs_species = aov(iris$Sepal.Length~iris$Species)
summary(sepal_length_vs_species)
# Because F value is high (119.3) and that means there 
# is a large difference among the groups and the p value says it's 
# extremely statistically significant <.001

# Let's do sepal width
sepal_width_vs_species = aov(iris$Sepal.Width~iris$Species)
summary(sepal_width_vs_species)
# Because F value is high (49.16) and that means there 
# is a large difference among the groups and the p value says it's 
# extremely statistically significant <.001

# Conclusion of ANOVA tests:all p values are significant and under 0.001, 
# meaning that there is significant difference in the 6 variables between 
# the three species groups.


# Now let's get a box plot of the six measurements subdivided by
# iris type.The box plots will be saved so we can see them later. 

# Let's do sepal area first
png(file="sepal_area_vs_species.png")
boxplot(iris$sepal_area~iris$Species)
dev.off()

# Let's do petal area first
png(file="petal_area_vs_species.png")
boxplot(iris$petal_area~iris$Species)
dev.off()

# Let's do sepal length first
png(file="sepal_length_vs_species.png")
boxplot(iris$Sepal.Length~iris$Species)
dev.off()


# Let's do sepal width first
png(file="sepal_width_vs_species.png")
boxplot(iris$Sepal.Width~iris$Species)
dev.off()

# Let's do petal length first
png(file="petal_length_vs_species.png")
boxplot(iris$Petal.Length~iris$Species)
dev.off()

# Let's do petal width first
png(file="petal_width_vs_species.png")
boxplot(iris$Petal.Width~iris$Species)
dev.off()

# Conclusion from boxplots: The boxplots display the median of values of the 
# variable in question grouped by species as well as the middle 50% 
# (25% above and below the median) which indicates the standard deviation, 
# as well as any outliers. From the box plots, we can see that petal area 
# between of the different species may be the best determining variable to 
# distinguish between the three species. This is because petal area has the 
# least overlapping values between quartiles.
