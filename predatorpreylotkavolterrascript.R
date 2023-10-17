install.packages("deSolve")
library(deSolve)
install.packages("ggplot2")
library(ggplot2)

LG <- function(t,state,parameters){ ##logistic grown function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dP <- r*(1-P/K)*P ##this is our logistic equation governing the rate of change of P
    
    return(list(dP)) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}

LV <- function(t,state,parameters){ ##logistic growth function,that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##looking for r, K and P in state and parameters
    dx <- alpha * x * (1-x/K) - (beta * x * y)/(1 + A * x) ##this is our logistic equation governing the rate of change of P
    dy <- (delta * x * y)/(1 + A * x) - gamma * y
    return(list(c(dx,dy))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}


state <- c(x = 10, y = 10)
parameters <- c(alpha = 0.1, beta = 0.02, delta = 0.02, gamma = 0.4, K = 30, A = 0.01)
times <- seq(0,500,by=1)
out <- ode(y=state, times = times, func = LV, parms = parameters)
out.df <- data.frame(out)

ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")
