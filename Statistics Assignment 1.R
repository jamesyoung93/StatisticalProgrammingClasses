#James Young - statistics assignment 1

#Problem 1
problem.1 <- read.csv("C:\\data\\datatab_5_12.csv")
str(problem.1)
#Split the data into the two differently taught classes
class <-split(problem.1,problem.1$ï..class)
#We now have class$a and class$b representing the two classes
#name the two classes for use in t test
class_A <-class$a
class_B <-class$b
#T test is default "one is signif. diff. than other"
t.test(class_A$output,class_B$output)

#Problem 2
problem.2 <- read.csv("C:\\data\\datatab_5_14.csv")
str(problem.2)
#Split the data into the two different diets
diet <-split(problem.2,problem.2$ï..diet)
diet
#We now have diet$new and diet$reg representing the two diets
#name the two diets for use in t test
New_Diet <-diet$new
Regular_Diet <-diet$reg
#T test is testing alternative hypothesis "New Diet puts more weight on cows 
# than regular diet"
t.test(New_Diet$weight,Regular_Diet$weight, alternative = "greater")
