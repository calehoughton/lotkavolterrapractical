x <- seq(0,50,0.1)
A <- 0.1 # Manipulated this value to find a suitable A for a type 2 functional response, for the rate at which predators can consume prey.
# I then input this A into my parameters
y <- x/(1+A*x)
ggplot()+
  geom_line(mapping=aes(x=x,y=y,color="blue")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed")
