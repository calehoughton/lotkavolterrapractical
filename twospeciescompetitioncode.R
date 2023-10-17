LV <- function(t,state,parameters){ ##logistic growth function,that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##looking for r, K and P in state and parameters
    dx1 <- r1 * x1 * (1-(x1+alpha12*x2)/K1)
    dx2 <- r2 * x2 * (1-(x2+alpha21*x1)/K2)
    return(list(c(dx1,dx2))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}

state<-c(x1=50,x2=10)
parameters <- c(r1 = 0.3, r2 = 0.3, alpha12 = 1, alpha21 = 0.9, K1 = 200, K2 = 250)
times <- seq(0,500,by=1)
out <- ode(y=state, times = times, func = LV, parms = parameters)
out.df <- data.frame(out)

ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x1),color="blue") +
  geom_line(mapping=aes(x=time,y=x2),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
