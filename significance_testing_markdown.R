#find bounds on a set of numbers, i.e., find the middle 80 - 95% of a range of values
#first sort the numbers, then determine the values bounded 

my.numbers <- c(4,0,-5,3,-4,-1,1,-3,2,-2)
print(my.numbers)

#sort numbers from smallest to largest

my.numbers.sorted <- sort(my.numbers)
print(my.numbers.sorted)

#range is from -5 to 4 

#want middle 80% of values, so 20% is excluded, 10% from upper/lower
#have 10 values total - so 10% of 10 is 1 -- exclude 1 number from upper and 1 from lower (2 to 9 included)

lower.bound <- my.numbers.sorted[2]
upper.bound <- my.numbers.sorted[9]

print(lower.bound)
print(upper.bound)

#lets learn the LOGIC OF SIGNIFICANCE TESTING ---- 

#Example: n=100, correlation = 0.21 -- what does it mean for a correlation to be significant 

#see handout! 

#CREATING A NULL HYPOTHESIS SAMPLING DISTRIBUTION 

library(learnSampling)

#specify sampling size & create NHSD 
#from a population with 100 people where correlation was 0, repeated 10000 times) 
samples.rho.zero <- get_cor_samples(rho=0, n=100,number.of.samples = 10000,number.of.decimals=4)

#see the average
mean(samples.rho.zero$r)

#whats the range of sample correlations where rho is zero, sort them first
#see the top 25 and lowest 25 - extreme values 

sorted.samples.rho.zero <- sort_samples_by_r(samples.rho.zero)

head(sorted.samples.rho.zero,25)
tail(sorted.samples.rho.zero,25)

#figure out the middle 95% of the NHSD - dont want 5%, 2.5% on either end, 10000 samples, so 250 each side

lower.bound <- sorted.samples.rho.zero[251, ]
upper.bound <- sorted.samples.rho.zero[9750, ]

print(lower.bound)
print(upper.bound)

#see output, value is -.20, +.20, this means that any correlation more than +/- 0.20 will be significant 

#we had a sample mean of .21, so it is significant 

#EXACT P-VALUE - extremity of a sample correlation in the NHSD ----------------------------
#exact p-value tells me what proportion of values in the NHSD are more extreme than .21 

number.more.extreme.one.side <- sum(sorted.samples.rho.zero$r>.21)
print(number.more.extreme.one.side)

#see output = 190, there are 190 correlations more extreme than +.21 

#look at both tails - just double - 190 x2 = 380

#determine the proportion 380/10000 = .038
#therefore, the 2 tailed p-value for a correlation of .21 is p=0.036 = small/mod positive correlation found


#another example 
head(example1)

#calculate correlation between self-esteem and height, get p value
cor.test(example1$SelfEsteem, example1$Height)

#example 2

head(example2)
cor.test(example2$SelfEsteem, example2$Height)

#r value = .21, p-value = .07 - check your findings by doing everything from above again 
samples.rho.zero <- get_cor_samples(rho = 0, n=75, number.of.samples = 10000, number.of.decimals = 4)

mean(samples.rho.zero$r)

sorted.samples.rho.zero <- sort_samples_by_r(samples.rho.zero)

head(sorted.samples.rho.zero,25)
tail(sorted.samples.rho.zero,25)

lower.bound <- sorted.samples.rho.zero[251,]
upper.bound <- sorted.samples.rho.zero[9750,]

print(lower.bound)
print(upper.bound)

number.more.extreme.one.side <- sum(sorted.samples.rho.zero$r>.21)
print(number.more.extreme.one.side)

#340 x 2 = 680 / 10000 = 0.068

#therefore, r = .21, p = .068

#matches - i did it right! 

