# Supress warnings
options(warn=-1)

# generate data from a uniform distribution
uniform_data <- runif(100000,0,100)   

# create a density plot
plot(density(uniform_data ))

hist(uniform_data)

qunif(p = 0.97,      # Probability cutoff
      min = 0, 
      max = 1)

random_ints <- floor(runif(100000,0,100))
hist(random_ints)




# Generate normally distributed data
normally_distributed <- rnorm(1000000,     
                              mean = 0,    
                              sd = 1)  
hist(normally_distributed)

library(ggplot2)




prob_under_minus1 <- pnorm(q=-1,        
                           mean=0,
                           sd=1)

# Get prob of observing a value over 1
prob_over_1 <-  1-pnorm(q=1,            
                        mean=0,
                        sd=1)

# Prob between -1 and 1
between_prob <- 1-(prob_under_minus1+prob_over_1)  






# Plot the density curve with the cutoff areas
norm_frame = with(density(normally_distributed),  # Create data frame density values
                  data.frame(x,y))  

myplot <- ggplot(data = norm_frame, aes(x = x, y = y)) +   # Create the plot
  geom_line() +
  geom_ribbon(data=subset(norm_frame,x < -1),
              aes(ymax=y, ymin=0),
              fill="red", 
              alpha=0.4) +
  geom_ribbon(data=subset(norm_frame,x > 1),
              aes(ymax=y, ymin=0),
              fill="red", 
              alpha=0.4) +
  geom_ribbon(data=subset(norm_frame,x > -1 & x < 1),
              aes(ymax=y, ymin=0),
              fill="skyblue", 
              alpha=0.4) +
  geom_text(x=-1.6,y=0.03,label=round(prob_under_minus1,4),size=4) +
  geom_text(x=1.6,y=0.03,label=round(prob_under_minus1,4),size=4) +
  geom_text(x=0,y=0.1,label=round(1-(prob_under_minus1*2),4),size=5) +
  xlim(-4,4)

myplot




#binomial distribution

fair_coin_flips   <- rbinom(1000000,    # Generate data from the binomial distribution
                            size = 10,   # With 10 trials
                            prob = 0.5)  # And success probability 0.5

table(fair_coin_flips)                # Check the counts

hist(fair_coin_flips, breaks=seq(-0.5,10.5,1))





# prepare distributions
set.seed(31415)
# Generate normally distributed data
normally_distributed <- rnorm(1000000,     
                              mean = 5,    
                              sd = 1)  
hist(normally_distributed)


# Generate data from a uniform distribution
uniform_distributed <- runif(1000000, 0, 10)   
hist(uniform_distributed)

#Generate binomial distribution symmetrical

binomial_sym_distributed  <- rbinom(1000000,    # Generate data from the binomial distribution
                            size = 10,   # With 10 trials
                            prob = 0.5)  # And success probability 0.5

hist(binomial_sym_distributed)


#Generate binomial distribution asymmetrical

binomial_asym_distributed  <- rbinom(1000000,    # Generate data from the binomial distribution
                                    size = 10,   # With 10 trials
                                    prob = 0.3)  # And success probability 0.5

hist(binomial_asym_distributed)

#Generate exponential distribution
exponential_distributed <- rexp(1000000, rate=1)
hist(exponential_distributed)

#combine into data.frame

df01<-data.frame(normally_distributed, uniform_distributed,
                 binomial_sym_distributed, binomial_asym_distributed,
                 exponential_distributed)
head(df01)

# convert to long data
library(tidyr)
library(dplyr)
library(ggplot2)
df02_long<-pivot_longer(df01, 1:5, names_to = "Distribution", values_to = "Value")



#histogram and density plots
ggplot(df02_long, aes(x=Value)) + geom_histogram()+
  facet_wrap(~Distribution, scales="free")

ggplot(df02_long, aes(x=Value)) + geom_histogram(aes(y=..density..))+
  facet_wrap(~Distribution, scales="free")+
  geom_density(colour= 2)+
  geom_vline(data = summary_df, aes(xintercept = mean), linetype = "dashed",
             colour = "red4")


summary_df<-df02_long %>%                               # Summary by group using dplyr
  group_by(Distribution) %>% 
  summarize(min = min(Value),
            q1 = quantile(Value, 0.25),
            median = median(Value),
            mean = mean(Value),
            q3 = quantile(Value, 0.75),
            max = max(Value)) 

vline_df <- data.frame(Distribution = levels(df02_long$Distribution),
                       Medians = tapply(X = df02_long$Value, INDEX = df02_long$Distribution,
                                        FUN = median))

a + geom_vline(data = vline_df, aes(xintercept = Medians), linetype = "dashed",
               colour = "red4")

#QQ-plots

x <- rnorm(1000)
df <- data.frame(x)
ggplot(df, aes(x = x)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()


#Boxplots

ggplot(df02_long, aes(x=Distribution, y=Value)) + 
  geom_boxplot()


#Violin plots
ggplot(df02_long, aes(x=Distribution, y=Value)) + 
  geom_violin()


ggplot(df02_long, aes(x=Distribution, y=Value)) + 
 geom_violin()+
 geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.02)