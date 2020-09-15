#####################
# EXERCISES
####################

################################################################
# 4.1 write what each line of the below code does
################################################################

show( HairEyeColor ) 
EyeHairFreq = apply( HairEyeColor, c("Eye","Hair"), sum ) # Sum across sex
EyeHairProp = EyeHairFreq / sum( EyeHairFreq ) # joint proportions, Table 4.1
show( round( EyeHairProp , 2 ) )
HairFreq = apply( HairEyeColor , c("Hair") , sum ) # Sum across sex and eye
HairProp = HairFreq / sum( HairFreq ) # marginal proportions, Table 4.1
show( round( HairProp , 2 ) )
EyeFreq = apply( HairEyeColor , c("Eye") , sum ) # Sum across sex and eye
EyeProp = EyeFreq / sum( EyeFreq ) # marginal proportions, Table 4.1
show( round( EyeProp , 2 ) )
EyeHairProp["Blue",] / EyeProp["Blue"] # conditional prob, Table 4.2
HairEyeColor

################################################################
# 4.2 replicate random coin flip, and change head prob to 0.8
################################################################

N = 500                   # Specify the total number of flips, denoted N.
pHeads = 0.8              # Specify underlying probability of heads.

# Generate a random sample of N flips (heads=1, tails=0):
flipSequence = sample( x = c(0,1), 
               prob = c(1-pHeads,pHeads),     # probability of heads and tails, if it is biased 1 - 0.6 will = 0.4
               size = N, replace=TRUE)

# Compute the running proportion of heads:
r = cumsum( flipSequence ) # Cumulative sum: Number of heads at each step.
n = 1:N                    # Number of flips at each step.
runProp = r / n            # Component by component division.

# Graph the running proportion:
plot( n , runProp , type="o" , log="x" , col="skyblue" ,
      xlim=c(1,N) , ylim=c(0.0,1.0) , cex.axis=1.5 ,
      xlab="Flip Number" , ylab="Proportion Heads" , cex.lab=1.5 ,
      main="Running Proportion of Heads" , cex.main=1.5 )

# Plot a dotted horizontal reference line:
abline( h=pHeads , lty="dotted" )

# Display the beginning of the flip sequence:
flipLetters = paste( c("T","H")[flipSequence[1:10]+1] , collapse="" )
displayString = paste0( "Flip Sequence = " , flipLetters , "..." )
text( N , .9 , displayString , adj=c(1,0.5) , cex=1.3 )

# Display the relative frequency at the end of the sequence.
text( N , .8 , paste("End Proportion =",runProp[N]) , adj=c(1,0.5) , cex=1.3 )

################################################################
# 4.3 Basic Probability
################################################################

# Deck of 48 cards, 4 suits, cards 9 - A and two of each card in each suit
# Probability of getting a 10 
# 8 / 48 = 19%
# Probability of getting a 10 or a J
# P(10) = 8 /48. P(J) = 8 /48. 8/48 + 8/48 = 0.166 + 0.166 
# = 32%

################################################################
# 4.4 Probability Density Function
################################################################

# probability density function of a biased spinner:
# p(x) = 6x(1 - x)
# over the interval:
# x E [0,1]

source("DBDA2E-utilities.R")
# Graph of normal probability density function, with comb of intervals.

xlow  = 0 # Specify low end of x-axis.
xhigh = 1 # Specify high end of x-axis.
dx = 0.1  # Specify interval width on x-axis
# Specify comb of points along the x axis:
x = seq( from = xlow , to = xhigh , by = dx )

# Compute y valyes, i.e, probability dentisty at each value of x
y = 6 * x * (1 - x)

# Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
openGraph(width=7,height=5)

plot( x , y, type="h" , lwd=1 , cex.axis=1.5
      , xlab="x" , ylab="p(x)" , cex.lab=1.5 
      , main="Probability Density: 6x(1-x)" , cex.main=1.5 )
lines( x , y , lwd=3 ,  col="skyblue" )

# Approximate the integral as the sum of width * height for each interval.
area = sum( dx * y )

# Display info in the graph.
text( 0.7 , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
      , adj=c(0,.5) , cex=1.5 )
text( 0.7 , .75*max(y) ,
      bquote(
      paste( sum(,x,) , " " , Delta , "x p(x) = " , .(signif(area,3)) )
      ) , adj=c(0,.5) , cex=1.5 )
# Save the plot to an EPS file.
saveGraph( file = "IntegralOfDensity" , type="eps" )


# From inspecting the graph what is the maximal value of p(x)?
# 1.5 when x(0.5)


################################################################
# 4.5 Area under normal distribution curve
################################################################

source("DBDA2E-utilities.R")
# Graph of normal probability density function, with comb of intervals.
meanval = 0.0 # Specify mean of distribution.
sdval = 0.2 # Specify standard deviation of distribution.
xlow = meanval - 1.0*sdval # Specify low end of x-axis.
xhigh = meanval + 1.0*sdval # Specify high end of x-axis.
dx = sdval/40 # Specify interval width on x-axis
# Specify comb of points along the x axis:
x = seq( from = xlow , to = xhigh , by = dx )
# Compute y values, i.e., probability density at each value of x:
y = ( 1/(sdval*sqrt(2*pi)) ) * exp( -.5 * ((x-meanval)/sdval)^2 )
# Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
openGraph(width=7,height=5)
plot( x , y , type="h" , lwd=1 , cex.axis=1.5
      , xlab="x" , ylab="p(x)" , cex.lab=1.5 ,
      , main="Truncated Normal Probability Density" , cex.main=1.5 )
lines( x , y , lwd=3 , col="skyblue" )
# Approximate the integral as the sum of width * height for each interval.
area = sum( dx * y )
# Display info in the graph.
text( meanval-0.5*sdval , .9*max(y) , bquote( paste(mu ," = " ,.(meanval)) )
      , adj=c(1,.5) , cex=1.5 )
text( meanval-0.5*sdval , .75*max(y) , bquote( paste(sigma ," = " ,.(sdval)) )
      , adj=c(1,.5) , cex=1.5 )
text( meanval+0.1*sdval , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
      , adj=c(0,.5) , cex=1.5 )
text( meanval+0.1*sdval , .75*max(y) ,
      bquote(
        paste( sum(,x,) , " " , Delta , "x p(x) = " , .(signif(area,3)) )
      ) , adj=c(0,.5) , cex=1.5 )



# (B) Now use the normal curve to describe the following belief. Suppose you believe that women’s heights follow a
# bell-shaped distribution, centered at 162 cm with about two-thirds of all women having heights between 147 and 177 cm.
# What should be the μ and σ parameter values?

#  u (mean) should equal 162 and sigma (SD) should equal 15 either side. Which would be 147 and 177



################################################################
# 4.6 Joint Probability
################################################################

# School children samples. 20% 1st grade, 20% 6th grade, 60% 11th grade
# Ice cream Fruit French fries
data.frame(
  grade    = c("1st", "6nd", "11th"),
  icecream = c(0.3, 0.6, 0.3),
  fruit    = c(0.6, 0.3, 0.1),
  chips    = c(0.1, 0.1, 0.6)
)

# From that information, construct a table of joint probabilities of grade and favorite food. 
# Also, say whether grade and favorite food are independent or not, and how you ascertained the answer. 
# Hint: You are given p(grade) and p(food|grade). You need to determine p(grade,food).

# joint_probabilities = data.frame(grade    = c("1st", "6nd", "11th"),
#                         icecream = (0.3 * 0.2), (0.6 * 0.2), (0.3 * 0.6),
#                         fruit    = (0.6 * 0.2), (0.3 * 0.2), (0.1 * 0.6),
#                         chips    = (0.1 * 0.2), (0.1 * 0.2), (0.6 * 0.6))

data.frame(grade = c("1st", "6th", "11th", "total"),
                                  icecream = c(0.06, 0.12, 0.18, 0.36),
                                  fruit = c(0.12, 0.06, 0.06, 0.24),
                                  chips = c(0.02, 0.02, 0.36, 0.40),
                                  total = c(0.20, 0.20, 0.60, 1.00))

# Food and grade are not independant (they are dependant), as if you run p(food, grade) for any cell, it does not
# equal p(food) * p(grade)
# example grade 6 and chips
# p(food, grade) = 0.02
# p(food) = 0.4 * p(grade) = 0.2 = 0.08
                 



################################################################
# 5.1 Bayes Rule, using posterior as next prior
################################################################

# Suppose that the same randomly selected person as in Table 5.4 gets re-tested after the first test result was
# positive, and on the re-test, the result is negative. When taking into account the results of both tests, what is the
# probability that the person has the disease?

# Specify hit rate of test:
p_positive_given_disease    <- 0.99

# Specify false alarm rate of test
p_positive_given_no_disease <- 0.05

# Specify original prior
p_disease                   <- 0.001

# Bayes Rule for first, positive test 
p_disease_give_positive <- p_positive_given_disease * p_disease / 
                           (p_positive_given_disease * p_disease + p_positive_given_no_disease * (1.0 - p_disease))

show(p_disease_give_positive)

####### Use the above posterior as the next calculations prior
# Set the prior to the new probability of have the diseae:
# p_disease = p_disease_given_positive

# Bayes Rule for second, negative test: 
p_disease_given_negative <- ( ( 1.0 - p_positive_given_disease) * p_disease /
                               ( ( 1.0 - p_positive_given_disease) * p_disease +
                                   (1.0 - p_positive_given_no_disease) * (1.0 - p_disease)))
show(p_disease_given_negative)



################################################################
# 5.2 (B) Bayes Rule calculation on conditional table
################################################################

# determine the proportion of people who have the disease, given that their test result is positive.
# intuitive answer
99
# Conditional Probability
# Bayes Rule = P(A|B) = P(B|A) / P(B)
# A = 100 
# B = 5094
99 / 5094



################################################################
# 6.1  Influence of Priors in each successfive flip 
################################################################
source("DBDA2Eprograms/DBDA2E-utilities.R")
source("DBDA2Eprograms/BernBeta.R")

# Flips H H T
post  <- BernBeta(priorBetaAB = c(4, 4), Data = c(1) )
post2 <- BernBeta(priorBetaAB = post, Data = c(1))   
post3 <- BernBeta(priorBetaAB = post2, Data = c(0))

# Flips T H H
ppost  <- BernBeta(priorBetaAB = c(4, 4), Data = c(0) )
ppost2 <- BernBeta(priorBetaAB = ppost, Data = c(1))   
ppost3 <- BernBeta(priorBetaAB = ppost2, Data = c(1))

# Regardlesss of the order of flips, the final posterior is the same


################################################################
# 6.2 Using HDI for Crediability Intervals 
################################################################

# 1 = Candidate A
# 0 = Candidate B
post <- BernBeta(priorBetaAB = c(1, 1), Data = c(rep(1, 58), rep(0, 100-58)),
                 showHDI = TRUE,
                 showCentTend =  TRUE)
                 BernBeta()
                 
# The 95% HDI is 0.483 - 0.673 for a probability of Candidate A
                 
# Running another poll of 100 people finds candidate A at 57
# What is the updated 95% HDI
                 
post2 <- BernBeta(priorBetaAB = post, Data = c(rep(1, 57), rep(0 , 100 - 57)),
                  showHDI = TRUE,
                  showCentTend = TRUE)

# The updated 95% HDI is 0.506 - 0.642 fo a probability of Candidate A



################################################################
# 6.3 Crediability between tests of particpants biases
################################################################

# n = 50
# radio by itself
# F = 40
# J = 10

# Start with a uniform prior. F = 1, J = 0
post <- BernBeta(priorBetaAB = c(1, 1), Data = c(rep(1, 40), rep(0, 10)),
                 showHDI = TRUE)

# Start with a uniform prior. F = 1, J = 0
# word combination
# F = 15
# J = 35

post2 <- BernBeta(priorBetaAB = c(1, 1), Data = c(rep(1, 15), rep(0, 35)),
                  showHDI = TRUE)

# Particpants were biased towards F in the first test as there was no ROPE around theta of 0.5, while in test two
# particpants were biased towards J, as there was no ROPE around the theta of 0.5 (which would indicate no bias)


################################################################
# 6.4 Biased magic coin
################################################################

post = BernBeta( priorBetaAB=c(1,1)/100 , Data=c(rep(1,4),rep(0,1)) ,
                 showHDI=TRUE , showCentTend="Mode" )
