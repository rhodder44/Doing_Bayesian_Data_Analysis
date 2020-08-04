#####################
# EXERCISES
####################

################################################################
# 4.2 write what each line of the below code does
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
# 4.3 replicate random coin flip, and change head prob to 0.8
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
